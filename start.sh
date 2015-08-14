#! /bin/bash

cp -r Home/* ~/

if [[ ! -f ~/.emacs.d/init.el ]]; then
   mkdir ~/.emacs.d/
   ln -s ~/Projects/Personal/myworkenv/init.el ~/.emacs.d/
fi
