# Homer
Dotfile management with git from inside Emacs.

<p align="center">
  <img src="https://upload.wikimedia.org/wikipedia/commons/1/1c/Homer_British_Museum.jpg" alt="Homer Thinking" width="50" height="100" />
</p>

# Motivation

One useful way to track dotfiles involves keeping a [git bare repository](https://www.atlassian.com/git/tutorials/dotfiles). This Emacs package builds upon this technique to make tracking these files using Emacs as easy as possible. The workflow here is as follows:

0. Create a remote repository to host your dotfiles on somewhere like GitHub or GitLab.

1. Run `M-x homeinit` to initialize homer. This will create a git bare repository in your home directory and initialize some files needed for homer to work.

Invoking `homeinit` will create a file called `dotfiles.dots` inside the directory `~/.config/homer/`, although you can change the path by editing the special variable `*homer-dotfile-path*` as you wish. The file `dotfiles.dots` describes the files you would like to track. It is structured as follows:

```
# This is a comment.
~/.config/emacs/init.el
~/.zshrc

# Blank lines are completely ok.

~/.xmonad/xmonad.hs
```

The `~` will be expanded into your home directory, just like a shell.

By default, `homer` will warn you if you have entries in your dotfiles file that don't exist. You can silence the warning (and, indeed, all other non-error messages homer emits) by setting the variable `*homer-silent-output*` to `t`.

2. Run `homeadd` at any point to stage changes to your dotfiles.

3. Run `homepush` to commit those changes and push them to the remote repository you configured initially.

On a day-to-day basis, you'll probably be running `homeadd` and `homepush` all the time; you will only invoke `homeinit` once per machine you use.

# Testing

Install [eldev](https://github.com/doublep/eldev) and run:

```sh
eldev test
```

From inside this directory.
