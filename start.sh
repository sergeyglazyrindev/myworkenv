#! /bin/bash


CUR_DIR=`pwd`
HOME_DIR=`dirname ~/`/$USER

# keep home directory structure the same everywhere
HOME_STRUCTURE_ARCHIVE="$CUR_DIR/home.tar.gz"
/bin/tar --skip-old-files -xvzf $HOME_STRUCTURE_ARCHIVE --directory $HOME_DIR/

# emacs beautifying
if [[ ! -h ~/.emacs.d/init.el ]]; then
   mkdir ~/.emacs.d/
   ln -s ~/Projects/Personal/myworkenv/init.el ~/.emacs.d/
fi
