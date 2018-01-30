# The normal zsh sigil for computing this is `${0:A:h}`, but it looks like that
# will not work for files that zsh sources as part of its startup sequence.
ROOT="$HOME/.dotfiles"

export ZSH_CACHE_DIR="$HOME/.cache/zsh"


# Functions
# =========

function source_if_possible()
{
    if [[ -r $1 ]]; then
        source $1
    fi
}

function abspath() {

    local usage=$(cat <<-'EOF'
	abspath: Resolve a relative path to an absolute path

	Usage:
	 abspath <path>

	Arguments:
	  path: A path

	Example:
	  abspath 'README.md' # --> /home/jsmith/README.md

	Notes:
	  Taken from http://stackoverflow.com/a/23002317/514210
	EOF
	)

    if ! (($+1)); then
	echo $usage
	return 1
    fi

    if [[ -d "$1" ]]; then
        # dir
        (cd "$1"; pwd)
    elif [[ -f "$1" ]]; then
        # file
        if [[ $1 == */* ]]; then
            echo "$(cd "${1%/*}"; pwd)/${1##*/}"
        else
            echo "$(pwd)/$1"
        fi
    fi
}

function eexists() {
    # Test if there's a command with the given name
    whence "$1" &> /dev/null
}

function pathdrop() {

    local usage=$(cat <<-'EOF'
	pathdrop: Remove items from the $path array that match the given pattern

	Usage:
	 pathdrop <pattern>

	Arguments:
	  pattern: A zsh pattern. Use quotes to prevent glob expansion

	Example:
	  pathdrop '*conda*'

	Notes:
	  - Manually edit the path using `vared path`
	  - Remove an element by index using `path[<ix>]=()`
	  - Append a new item using `path+=<item>`
	  - Prepend a new item using `path=(<item> $path)`
	EOF
	)

    if ! (($+1)); then
	echo $usage
	return 1
    fi

    # http://zsh.sourceforge.net/Doc/Release/Expansion.html#Parameter-Expansion

    # When x is an array, ${x:#<pattern>} will exclude elements of x that match
    # the pattern.

    # ${~x} is supposed to turn *on* the GLOB_SUBST option for the expansion of
    # x. It seems like it's actually turning the GLOB_SUBST option *off*,
    # though.

    path=(${path:#${~1}})
    echo $path

    # Since `PATH` and `path` are already tied together, and already marked as
    # exported variables, subprocesses launched by this shell will see the
    # updated value.

    return 0
}

# Plugins
# =======

source_if_possible "${ROOT}/zsh_plugins/liquidprompt/liquidprompt"

source_if_possible "${ROOT}/zsh_plugins/fasd/fasd"
eexists fasd && eval "$(fasd --init auto)"

source_if_possible "${ROOT}/zsh_plugins/zaw/zaw.zsh"

source_if_possible "${ROOT}/zsh_plugins/zsh-autosuggestions/zsh-autosuggestions.zsh"


# GNU Coreutils
# =============

# If GNU coreutils are installed, we'd like those to be available for
# interactive use. These binaries are installed by Homebrew into `$(brew
# --prefix)/bin` under names prefixed by 'g', so we get 'gls', 'gmkdir', and so
# on. There is another directory that contains non-prefixed versions of each of
# these binaries as well, and adding it to the path is one way to go. However,
# sometimes build tools complain about non-prefixed coreutils on PATH, so this
# aliasing method is a different way to tackle it.

if eexists brew && [[ -d $(brew --prefix)/opt/coreutils/libexec/gnubin ]]; then
    for cmd in $( ls $(brew --prefix)/opt/coreutils/libexec/gnubin ); do
        alias "${cmd}"="g${cmd}"
    done
    # There was some sort of issue with using an alias with '['.
    unalias "["
    alias ls="gls --color=auto"
elif ls --color=auto /dev/null &> /dev/null; then
    alias ls="ls --color=auto"
fi


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

alias lsa="ls --almost-all"
alias ll="ls -l --human-readable"
alias la="ls -l --almost-all --human-readable"
alias lst="tree -phugDC"
alias lst2="lst -L 2"

alias dirsize="du --human-readable --max-depth=1 --exclude='./.*' | sort --human-numeric-sort --reverse"
alias dirsizeall="du --human-readable --max-depth=1 | sort --human-numeric-sort --reverse"


# Completion
# ==========

eexists brew && fpath=("$(brew --prefix)/share/zsh/site-functions" $fpath)
fpath=("${ROOT}/zsh_plugins/zsh-completions/src" $fpath)

autoload -U compinit

# Copied verbatim from
# https://github.com/robbyrussell/oh-my-zsh/blob/master/lib/completion.zsh

unsetopt menu_complete   # do not autoselect the first completion entry
unsetopt flowcontrol
setopt auto_menu         # show completion menu on succesive tab press
setopt complete_in_word
setopt always_to_end

WORDCHARS=''

zmodload -i zsh/complist

zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

zstyle ':completion:*' list-colors ''

# should this be in keybindings?
bindkey -M menuselect '^o' accept-and-infer-next-history

zstyle ':completion:*:*:*:*:*' menu select
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,comm -w -w"

# disable named-directories autocompletion
zstyle ':completion:*:cd:*' tag-order local-directories directory-stack path-directories

# Use caching so that commands like apt and dpkg complete are useable
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion::complete:*' cache-path $ZSH_CACHE_DIR

# Don't complete uninteresting users
zstyle ':completion:*:*:*:users' ignored-patterns \
        adm amanda apache at avahi avahi-autoipd beaglidx bin cacti canna \
        clamav daemon dbus distcache dnsmasq dovecot fax ftp games gdm \
        gkrellmd gopher hacluster haldaemon halt hsqldb ident junkbust kdm \
        ldap lp mail mailman mailnull man messagebus  mldonkey mysql nagios \
        named netdump news nfsnobody nobody nscd ntp nut nx obsrun openvpn \
        operator pcap polkitd postfix postgres privoxy pulse pvm quagga radvd \
        rpc rpcuser rpm rtkit scard shutdown squid sshd statd svn sync tftp \
        usbmux uucp vcsa wwwrun xfs '_*'

# ... unless we really want to.
zstyle '*' single-ignored show

compinit


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

autoload -U up-line-or-beginning-search
zle -N up-line-or-beginning-search
bindkey "^[[A" up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey "^[[B" down-line-or-beginning-search


# Other
# =====

# Perform implicit tees or cats when multiple redirections are attempted (see
# the section 'Redirection').
setopt multios

# Equivalent to `typeset -fuU zmv`; `-U` suppresses alias expansion when the
# function definition is read
autoload -U zmv

export EDITOR=emacs

alias glog="git log --oneline --decorate --graph"

alias wr="whence -v"

alias conac="source activate"
alias conde="source deactivate"


# Local settings
# ==============

source_if_possible "$HOME/.local_zshrc"
