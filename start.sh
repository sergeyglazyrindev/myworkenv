#! /bin/bash

CUR_DIR=`pwd`
HOME_DIR=`dirname ~/`/$USER
HOME_STRUCTURE_ARCHIVE="$CUR_DIR/home.tar.gz"
/bin/tar --skip-old-files -xvzf $HOME_STRUCTURE_ARCHIVE --directory $HOME_DIR/

if [[ ! -h ~/.emacs.d/init.el ]]; then
   mkdir ~/.emacs.d/
   ln -s ~/Projects/Personal/myworkenv/init.el ~/.emacs.d/
fi
