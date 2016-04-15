#! /bin/bash

# this is for powerline-shell python3 support
# more here https://github.com/fellipecastro/powerline-shell/commit/49e00844774f4dd34365e9c9981526b3742c8fbd
alias python2='python'

function _update_ps1() {
    PS1="$(~/Projects/Repos/powerline-shell/powerline-shell.py $? 2> /dev/null)"
}

# Support for bash
if [ "$TERM" != "linux" ]; then
    PROMPT_COMMAND="prompt; _update_ps1"
fi

function prompt()
{
    if [ "$PWD" != "$MYOLDPWD" ]; then
	MYOLDPWD="$PWD"
	test -e .venv && workon `cat .venv`
	test -e .dir.sh && source .dir.sh
    fi
}

cd () {
    if (( $# == 0 ))
    then
	builtin cd $VIRTUAL_ENV
    else
	builtin cd "$@"
    fi
}

