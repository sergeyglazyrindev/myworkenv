#! /bin/bash

REPO_DIR="$HOME/Projects/Repos/konsole-colors-solarized"
if [ ! -d $REPO_DIR ]; then
    git clone https://github.com/phiggins/konsole-colors-solarized.git $REPO_DIR
    cp $REPO_DIR/Solarized* $HOME/.kde4/share/apps/konsole/
    std_alert "Please don't forget to activate konsole solarized colorscheme"    
fi


