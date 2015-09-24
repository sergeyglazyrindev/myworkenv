#! /bin/bash

# uninstall old stuff 
cp ~/.bashrc_mine ~/.bashrc_mine.bkp
cp ./bashrc_mine ~/.bashrc_mine
mkdir -p ~/Projects/ReposBkp/
echo "Backup repos to make sure nothing has lost"
cp -r ~/Projects/Repos/* ~/Projects/ReposBkp/ 2>/dev/null
rm -rf ~/Projects/Repos/*
# finished uninstalling

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
/bin/tar --skip-old-files -xzf $HOME_STRUCTURE_ARCHIVE --directory $HOME/ 2>/dev/null

# emacs beautifying
if [[ ! -h ~/.emacs.d/init.el ]]; then
   mkdir ~/.emacs.d/
   ln -s ~/Projects/Personal/myworkenv/init.el ~/.emacs.d/
fi

run_scripts_queue=(konsole powerline-shell gitsetup rbenv flymake)
for script in ${run_scripts_queue[@]}; do
    . includes/$script.sh
done
