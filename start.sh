#! /bin/bash

std_alert(){
    echo -e "\033[0;31m${1}\033[0m" >&2
}

update_bashrc() {
    if [ ! -f ~/.bashrc_mine ]; then
	touch ~/.bashrc_mine
	echo ". ~/.bashrc_mine" >> ~/.bashrc
    fi
    echo "Updating ~/.bashrc_mine. Adding there source file $1 to execute"
    echo ". ~/Projects/Personal/myworkenv/includes/__$1.sh" >> ~/.bashrc_mine
    echo "Updated ~/.bashrc_mine"
}

CUR_DIR=`pwd`

# keep home directory structure the same everywhere
HOME_STRUCTURE_ARCHIVE="$CUR_DIR/home.tar.gz"
/bin/tar --skip-old-files -xvzf $HOME_STRUCTURE_ARCHIVE --directory $HOME/

# emacs beautifying
if [[ ! -h ~/.emacs.d/init.el ]]; then
   mkdir ~/.emacs.d/
   ln -s ~/Projects/Personal/myworkenv/init.el ~/.emacs.d/
fi

run_scripts_queue=(konsole powerline-shell)
for script in ${run_scripts_queue[@]}; do
    . includes/$script.sh
done
