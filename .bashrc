# .bashrc

source_if_possible()
{
    if [[ -r $1 ]]; then
        source $1
    fi
}

# Source global definitions if they exist
source_if_possible /etc/bashrc

# Easier navigation: .., ..., ...., ....., ~ and -
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias -- -="cd -"

# Better file listings
case $OSTYPE in
darwin*)
    alias ls='ls -hFG' # Human-readable, classiFy, color output
    alias lsa='ls -A'  # Almost all
    alias l='ls -o'    # Omit group name
    alias la='l -A'
    ;;
*)
    alias ls='ls --no-group --human-readable --classify --color=auto'
    alias lsa='ls --almost-all'
    alias l='ls -l'
    alias la='l --almost-all'
    ;;
esac
alias ld='l | grep "^d"'
alias lda='la | grep "^d"'

# History control
export HISTIGNORE="&:ls:cd:cd -:pwd:exit:logout:date:* --help"
export HISTSIZE=32768
export HISTFILESIZE=$HISTSIZE
export HISTCONTROL=ignoreboth

# Nicer options for Bash. See
# http://www.gnu.org/software/bash/manual/html_node/The-Shopt-Builtin.html
shopt -s histappend checkwinsize nocaseglob cdspell autocd globstar &> /dev/null

# Only load Liquid Prompt in interactive shells, not from a script or from scp
if [[ $- = *i* ]]; then
    source_if_possible "${HOME}/liquidprompt/liquidprompt"
fi

source_if_possible "${HOME}/.bashrc_local"
