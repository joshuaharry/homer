;;; homer.el --- Manage your home directory from inside Emacs.  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Joshua Hoeflich

;; Author:     Joshua Hoeflich <joshuaharry411@icloud.com>
;; Maintainer: Joshua Hoeflich <joshuaharry411@icloud.com>
;; Version:    0.1.0
;; Keywords:   emacs, home
;; Homepage:   https://github.com/joshuaharry/homer.el
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;;; Use these functions to manage your dotfiles:
;;; - M-x homeinit will initialize homer.
;;; - M-x homeadd will put run the equivalent of "git add" on every file listed
;;; in the *homer-dotfile-list* file.
;;; - M-x homepush will push all of your dotfiles to the remote origin you specified
;;    when setting up homeinit for the first time.

;;; Code:
(defun homer--path-join (root &rest dirs)
  "Join ROOT with DIRS to construct a path string in an OS independent way."
  (let ((res root))
    (dolist (el dirs res)
      (setq res (expand-file-name el res)))))

(defvar *homer-git-dir-name*
  ".dotfiles"
  "Specify the folder name in $HOME that will contains the git bare repository.")

(defvar *homer-remote-branch-name* "main"
  "Default branch name for homer; defaults to main.")

(defvar *homer-silent-output*
  nil
  "Decide whether homer should output messages in normal operation.")

(defvar *homer-dotfile-path*
  (homer--path-join (getenv "HOME") ".config" "homer" "dotfiles.dots")
  "A path to the location of a file with dotfiles.")

(defun homer--random-string ()
  "Generate a random string of 10 characters."
  (let ((alnum "abcdefghijklmnopqrstuvwxyz0123456789")
	(res ""))
    (dotimes (_ 10 res)
      (let ((i (% (abs (random)) (length alnum))))
	(setq res (concat res (substring alnum i (1+ i))))))))

(defun homer--git-work-tree ()
  "Specify the work-tree option that homer needs."
  (concat "--work-tree=" (getenv "HOME")))

(defun homer--git-bare-path ()
  "Yield the path to the git bare repository homer interacts with."
  (homer--path-join (getenv "HOME") *homer-git-dir-name* ".git"))

(defun homer--git-init-command ()
  "Generate the command that yields the git bare repository for homer to use."
  `("git" "init" "--bare" ,(homer--git-bare-path)))

(defun homer--git-dir ()
  "Specify the git-dir option that homer needs."
  (concat "--git-dir=" (homer--git-bare-path)))

(defun homer--git-config-command ()
  "Generate the command that configures the git bare repository for homer to use."
  `("git" ,(homer--git-dir) ,(homer--git-work-tree)
    "config" "--local" "status.showUntrackedFiles" "no"))

(defun homer--git-remote-command (remote)
  "Add origin to the URL REMOTE specified."
  `("git" ,(homer--git-dir) ,(homer--git-work-tree) "remote" "add" "origin" ,remote))

(defun homer--git-add-command (line)
  "Generate a git add command given the LINE of a file to add."
  `("git" ,(homer--git-dir) ,(homer--git-work-tree) "add" ,line))

(defun homer--git-commit-command (message)
  "Generate a git commit command given the MESSAGE to use."
  `("git" ,(homer--git-dir) ,(homer--git-work-tree) "commit" "-m" ,message "--allow-empty"))

(defun homer--git-push-command ()
  "Generate a git push command that will push your code to a branch."
  `("git" ,(homer--git-dir) ,(homer--git-work-tree)
    "push" "--set-upstream" "origin" ,*homer-remote-branch-name*))

(defun homer--command-line (acc line)
  "Transform LINE into an executable command and attach it to ACC."
  (let ((the-path (replace-regexp-in-string
		   "^~" (getenv "HOME") (car (split-string line " ")))))
    (cond ((or (string= "" the-path) (string= "#" the-path)) acc)
	  ((not (file-exists-p the-path))
	   (unless *homer-silent-output*
	     (message "Warning: Could not find file %s, skipping" the-path))
	   acc)
	  (:good-to-go
	   (cons (homer--git-add-command the-path) acc)))))

(defun homer--parse-dotfiles (lines)
  "Parse the raw LINES into a list of executable commands."
  (seq-reduce #'homer--command-line lines nil))

(defun homer--read-dotfiles-to-lines ()
  "Read the user's dotfiles.dots file into a list of strings."
  (unless (file-exists-p *homer-dotfile-path*)
    (error "Could not find file %s - did you run homeinit?"
	   *homer-dotfile-path*))
  (with-temp-buffer
    (insert-file-contents *homer-dotfile-path*)
    (split-string (buffer-string) "\n" t)))

(defun homer--read-dotfiles-to-commands ()
  "Read the user's dotfiles.dots file into a list of git add commands."
  (homer--parse-dotfiles
   (homer--read-dotfiles-to-lines)))

(defun homer--make-generator (el)
  "Create a function that yields every element in EL.
After reaching the end of the list, the function will always return nil."
  (let ((cur el))
    (lambda ()
      (let ((arg (car cur)))
	(setq cur (cdr cur))
	arg))))

(defun homer--shell-and-then (commands error-message then)
  "Run the shell command specified by the COMMANDS list;
invoke THEN if the command succeeds, or error with ERROR-MESSAGE."
  (let ((buf (generate-new-buffer (concat " *" (homer--random-string) "*")))
	(cmd (mapconcat #'identity commands " ")))
    (condition-case nil
	(set-process-sentinel
	 (apply (apply-partially #'start-process cmd buf) commands)
	 (lambda (process _)
	   (cond ((= 0 (process-exit-status process))
		  (kill-buffer buf)
		  (funcall then))
		 (:otherwise
		  (error error-message)))))
      (error (error error-message)))))

(defun homer--write-default-config-file ()
  "Write the default dotfiles configuration file."
  (unless (file-exists-p *homer-dotfile-path*)
    (with-temp-buffer
      (insert
       (replace-regexp-in-string
	(concat "^" (getenv "HOME"))
	"~" *homer-dotfile-path*))
      (insert "\n")
      (write-file *homer-dotfile-path* nil))))

(defun homer--push-files (commit-message)
  "Push all of the files homer is tracking with the provided COMMIT-MESSAGE."
  (homer--shell-and-then
   (homer--git-commit-command commit-message)
   "Could not create commit"
   (lambda ()
     (homer--shell-and-then
      (homer--git-push-command)
      "Could not push to origin"
      (lambda () (message "Pushed dotfiles to remote successfully"))))))

(defun homeinit ()
  "Initialize homer for the first time.
If you have already ran this function or you already have an existing git bare
repository in your home directory, this function will likely throw an error."
  (interactive)
  (let ((remote (read-string "Enter the URL where your repository lives: ")))
    (homer--write-default-config-file)
    (homer--shell-and-then
     (homer--git-init-command)
     "Could not initialize git repository - have you already run homeinit?"
     (lambda ()
       (homer--shell-and-then
	(homer--git-config-command)
	"Could not configure git repository - have you already run homeinit?"
	(lambda ()
	  (print (homer--git-remote-command remote))
	  (homer--shell-and-then
	   (homer--git-remote-command remote)
	   "Could not add origin to git repository - have you already run homeinit?"
	   (lambda ()
	     (unless *homer-silent-output*
	       (message "Homer configured successfully."))))))))))

(defun homeadd (&optional and-then)
  "Stage your dotfiles; run AND-THEN when it is provided."
  (interactive)
  (letrec ((dots (homer--read-dotfiles-to-commands))
	   (gen (homer--make-generator dots))
	   (error-message "Attempting git add failed")
	   (do-next
	    (lambda ()
	      (let ((next (funcall gen)))
		(if next (homer--shell-and-then next error-message do-next)
		  (when and-then
		    (funcall and-then)))))))
    (funcall do-next)))

(defun homepush ()
  "Push the state of your home directory to remote."
  (interactive)
  (let ((commit-message (read-string "Enter a commit message: ")))
    (homeadd
     (lambda ()
       (homer--push-files commit-message)))))

(provide 'homer)
;;; homer.el ends here
