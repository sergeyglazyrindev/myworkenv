#! /bin/bash

REPO_DIR="$HOME/Projects/Repos/powerline-shell"
if [ ! -d $REPO_DIR ]; then
    git clone https://github.com/milkbikis/powerline-shell.git $REPO_DIR
    cp $REPO_DIR/config.py.dist $REPO_DIR/config.py
    CUR_DIR=`pwd`
    cd $REPO_DIR
    ./install.py
    
    # fix for python 3 virtual environments
    # more here https://github.com/fellipecastro/powerline-shell/commit/49e00844774f4dd34365e9c9981526b3742c8fbd
    sed -i '1s/python$/python2/' powerline-shell.py
    # endfix for python3 virtual environments
    
    cd $CUR_DIR
    update_bashrc "powerline-shell"
    std_alert "Don't forget to install virtualenvwrapper for your system"
fi
