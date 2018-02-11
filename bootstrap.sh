#!/bin/sh

mkdir -p ~/.emacs.d

ln -sf $PWD/.bashrc      ~
ln -sf $PWD/.screenrc    ~
ln -sf $PWD/.tmux.conf   ~
ln -sf $PWD/settings.org ~/.emacs.d/
ln -sf $PWD/custom.el    ~/.emacs.d/
cp  -f $PWD/init.el      ~/.emacs.d/
