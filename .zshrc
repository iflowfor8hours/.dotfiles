source_if_possible()
{
    if [[ -r $1 ]]; then
        source $1
    fi
}

# Path to your oh-my-zsh installation.
export ZSH="${HOME}/.oh-my-zsh"

# Set name of the theme to load. Look in ~/.oh-my-zsh/themes/. Optionally, if
# you set this to "random", it'll load a random theme each time that oh-my-zsh
# is loaded.
ZSH_THEME=""

# User configuration

source $ZSH/oh-my-zsh.sh

# Equivalent to `typeset -fuU zmv`; `-U` suppresses alias expansion when the
# function definition is read
autoload -U zmv

source_if_possible ~/liquidprompt/liquidprompt

export EDITOR="emacs"

# Always enable colored `grep` output
export GREP_OPTIONS="--color=auto"

# Get a sorted list of subfolder sizes, as long as `sort` supports the `-h` option
alias dirsize="du --human-readable --max-depth=1 --exclude='./.*' | sort --human-numeric-sort --reverse"
alias dirsizeall="du --human-readable --max-depth=1 | sort --human-numeric-sort --reverse"

# Complement `whoami`
alias whereami="uname -n"

source_if_possible ~/.zshrc_local
