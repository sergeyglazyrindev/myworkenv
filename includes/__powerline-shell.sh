#! /bin/bash

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

