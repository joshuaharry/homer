(require 'buttercup)
(require 'homer)

(setq *homer-silent-output* t)

(describe "Generating random strings"
  (it "Should make 10 random characters"
    (let ((rand (homer--random-string)))
      (expect (length rand) :to-equal 10)
      (expect (stringp rand) :to-be t))))

(describe "Our generator"
  (it "Should return the strings we provide it"
    (let ((generator (homer--make-generator '("this" "is" "a" "test"))))
      (expect (funcall generator) :to-equal "this")
      (expect (funcall generator) :to-equal "is")
      (expect (funcall generator) :to-equal "a")
      (expect (funcall generator) :to-equal "test")
      (expect (funcall generator) :to-be nil)
      (expect (funcall generator) :to-be nil)))
  (it "Should work with non-string elements"
    (let ((generator (homer--make-generator '(foo ("1" "2" "3") bar))))
      (expect (funcall generator) :to-equal 'foo)
      (expect (funcall generator) :to-equal '("1" "2" "3"))
      (expect (funcall generator) :to-equal 'bar)
      (expect (funcall generator) :to-equal nil)
      (expect (funcall generator) :to-be nil)
      (expect (funcall generator) :to-be nil))))

(describe "Our path joining"
  (it "Should join strings correctly on Unix"
    ;; This test will fail on windows because the path string is
    ;; different, but that's OK, since I expect the vast majority
    ;; of users to be on OS X or some variant of Linux. :)
    (expect (homer--path-join "/" "var" "tmp") :to-equal "/var/tmp")))

(describe "Parsing dotfiles.dots"
  (it "Should work on comments/blank lines"
    (expect (homer--command-line nil "") :to-be nil)
    (expect (homer--command-line nil "# this is a comment") :to-be nil))
  (it "Should work on files that do not exist"
    (expect (homer--command-line nil "/does-not-exist") :to-be nil))
  ;; This test will fail if /bin/ls doesn't exist, but since I imagine
  ;; most Unixes will have it, I think this is ok.
  (it "Should work on files that do exist"
    (let ((res (car (homer--command-line nil "/bin/ls"))))
      (expect (car res) :to-equal "git")
      (expect (seq-every-p #'stringp res) :to-be t))))
