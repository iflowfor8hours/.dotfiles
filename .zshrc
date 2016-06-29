source_if_possible()
{
    if [[ -r $1 ]]; then
        source $1
    fi
}

function abspath() {
    # generate absolute path from relative path
    # $1     : relative filename
    # return : absolute path
    # From http://stackoverflow.com/a/23002317/514210
    if [ -d "$1" ]; then
        # dir
        (cd "$1"; pwd)
    elif [ -f "$1" ]; then
        # file
        if [[ $1 == */* ]]; then
            echo "$(cd "${1%/*}"; pwd)/${1##*/}"
        else
            echo "$(pwd)/$1"
        fi
    fi
}

# Directories
# ===========

setopt auto_cd
setopt auto_pushd
setopt pushd_ignore_dups
setopt pushdminus

alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../..'
alias -g ......='../../../../..'

alias -- -='cd -'
alias 1='cd -'
alias 2='cd -2'
alias 3='cd -3'
alias 4='cd -4'
alias 5='cd -5'
alias 6='cd -6'
alias 7='cd -7'
alias 8='cd -8'
alias 9='cd -9'

alias md='mkdir -p'
alias rd=rmdir
alias d='dirs -v | head -10'

# Look for a GNU version of ls
if $(gls --color -d . &> /dev/null); then
    eval $(gdircolors)
    alias ls="gls --color=auto"
elif $(ls --color -d . &> /dev/null); then
    eval $(dircolors)
    alias ls="ls --color=auto"
else
    alias ls="ls -G"
fi

alias lsa="ls --almost-all"
alias ll="ls -l --human-readable"
alias la="ls -l --almost-all --human-readable"

# Push and pop directories on directory stack
alias pu='pushd'
alias po='popd'

# Get a sorted list of subfolder sizes
if hash gsort 2> /dev/null; then
    alias du="gdu"
    alias sort="gsort"
fi
alias dirsize="du --human-readable --max-depth=1 --exclude='./.*' | sort --human-numeric-sort --reverse"
alias dirsizeall="du --human-readable --max-depth=1 | sort --human-numeric-sort --reverse"


# Autocorrect
# ===========

alias man='nocorrect man'
alias mkdir='nocorrect mkdir'
alias mv='nocorrect mv'
alias mysql='nocorrect mysql'
alias sudo='nocorrect sudo'

setopt correct_all


# History
# =======

HISTFILE="$HOME/.zsh_history"
HISTSIZE=10000
SAVEHIST=10000

setopt append_history
setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_dups # ignore duplication command history list
setopt hist_ignore_space
setopt hist_verify
setopt inc_append_history
setopt share_history # share command history data


# Prompt
# ======

# Loads support for setting colors?
autoload -U colors && colors

# If set, parameter expansion, command substitution and arithmetic expansion are
# performed in prompts. Substitutions within prompts do not affect the command
# status.
setopt prompt_subst


# Other
# =====

# Perform implicit tees or cats when multiple redirections are attempted (see
# the section 'Redirection').
setopt multios

# Equivalent to `typeset -fuU zmv`; `-U` suppresses alias expansion when the
# function definition is read
autoload -U zmv

export EDITOR=edit

alias glog="git log --oneline --decorate --graph"

# Complement `whoami`
alias whereami="uname -n"


# Local settings
# ==============

source_if_possible ~/.zshrc_local
