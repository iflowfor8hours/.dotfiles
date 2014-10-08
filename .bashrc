## Path

# Give precedence to Homebrew-managed tools
# /usr/local/bin is on the default path, but it appears after /usr/bin
export PATH="~/Library/Scripts:/usr/local/bin:${PATH}"


## Aliases

# Easier navigation: .., ..., ...., ....., ~ and -
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias -- -="cd -"

# Shortcuts
alias dl="cd ~/Downloads"
alias dt="cd ~/Desktop"
alias g="git"
alias h="history"
alias e="emacs --no-window-system"
alias v="vim"
alias s="subl ."

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

# View HTTP traffic
alias sniff="sudo ngrep -d 'en1' -t '^(GET|POST) ' 'tcp and port 80'"
alias httpdump="sudo tcpdump -i en1 -n -s 0 -w - | grep -a -o -E \"Host\: .*|GET \/.*\""

# Canonical hex dump; some systems have this symlinked
command -v hd > /dev/null || alias hd="hexdump -C"


## Functions

# Determine size of a file or total size of a directory
function fs() {
    if du -b /dev/null > /dev/null 2>&1; then
        local arg=-sbh
    else
        local arg=-sh
    fi
    if [[ -n "$@" ]]; then
        du $arg -- "$@"
    else
        du $arg .[^.]* *
    fi
}

# Jumping to location of frontmost Finder window
cdf() {
    target=`osascript -e 'tell application "Finder" to if (count of Finder windows) > 0 then get POSIX path of (target of front Finder window as text)'`
    if [ "$target" != "" ]; then
        cd "$target"; pwd
    else
        echo 'No Finder window found' >&2
    fi
}
alias f='open -a Finder ./'


## Completion

# Add tab completion for SSH hostnames based on ~/.ssh/config, ignoring wildcards
[ -e "$HOME/.ssh/config" ] && complete -o "default" -o "nospace" -W "$(grep "^Host" ~/.ssh/config | grep -v "[?*]" | cut -d " " -f2 | tr ' ' '\n')" scp sftp ssh

# bash-completion on Mac OS X
if [ -f $(brew --prefix)/etc/bash_completion ]; then
  . $(brew --prefix)/etc/bash_completion
fi

# Add bash completion for interactive shells (Ubuntu)
if ! shopt -oq posix; then
 if [ -f /usr/share/bash-completion/bash_completion ]; then
   . /usr/share/bash-completion/bash_completion
 elif [ -f /etc/bash_completion ]; then
   . /etc/bash_completion
 fi
fi

## Every other random-ass thing

export EDITOR="emacs"

[[ -r /usr/share/autojump/autojump.sh ]] && source /usr/share/autojump/autojump.sh

# History control
export HISTIGNORE="&:ls:cd:cd -:pwd:exit:logout:date:* --help"
export HISTSIZE=32768
export HISTFILESIZE=$HISTSIZE
export HISTCONTROL=ignoreboth

# Prefer US English and use UTF-8
export LANG="en_US"
export LC_ALL="en_US.UTF-8"

# Always enable colored `grep` output
export GREP_OPTIONS="--color=auto"

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# Nicer options for Bash. See
# http://www.gnu.org/software/bash/manual/html_node/The-Shopt-Builtin.html
shopt -s histappend checkwinsize nocaseglob cdspell autocd globstar &> /dev/null
