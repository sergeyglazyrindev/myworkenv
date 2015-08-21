#! /bin/bash

REPO_DIR="$HOME/Projects/Repos/powerline-shell"
if [ ! -d $REPO_DIR ]; then
    git clone https://github.com/milkbikis/powerline-shell.git $REPO_DIR
    cp $REPO_DIR/config.py.dist $REPO_DIR/config.py
    CUR_DIR=`pwd`
    cd $REPO_DIR
    ./install.py
    cd $CUR_DIR
    update_bashrc "powerline-shell"
    std_alert "Don't forget to install virtualenvwrapper for your system"
fi
