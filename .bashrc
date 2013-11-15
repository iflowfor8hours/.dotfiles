## Path
export PATH="~/bin:~/bin/datatools:${PATH}"

# Give precedence to Homebrew-managed tools
# /usr/local/bin is on the default path, but it appears after /usr/bin
export PATH="/usr/local/bin:${PATH}"
# And the Homebrew Ruby 2.0 gem
export PATH="/usr/local/opt/ruby/bin:$PATH"

## Aliases
# Easier navigation: .., ..., ...., ....., ~ and -
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias ~="cd ~" # `cd` is probably faster to type though
alias -- -="cd -"

# Shortcuts
alias dl="cd ~/Downloads"
alias dt="cd ~/Desktop"
alias g="git"
alias h="history"
alias e="emacs --no-window-system"
alias v="vim"
alias s="subl ."

alias ls='ls -G' # Color output

alias l='ls -lhFG'
alias la='ls -lahFG'

alias ld='ls -lhG | grep "^d"'
alias lda='ls -lahG | grep "^d"'

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

# Pretty-printers for man pages
manp()
{
    man -t "${1}" | open -f -a Skim
}
mantest()
{
    # (cd /usr/share/man && (/usr/bin/gunzip -c `man -w ${1}` 2>/dev/null || cat `man -w ${1}`) | /usr/bin/tbl | /usr/bin/groff -Tps -dpaper=letter -P-p10.5i,8i -mandoc -c) | open -f -a Skim
    (cd /usr/share/man && (/usr/bin/gunzip -c `man -w ${1}` 2>/dev/null || cat `man -w ${1}`) | /usr/bin/tbl | /usr/bin/groff -Thtml -mandoc -c)
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

[[ -r $(brew --prefix)/etc/bash_completion ]] && source $(brew --prefix)/etc/bash_completion

## Every other random-ass thing
export EDITOR="emacs"

# History control
export HISTIGNORE="&:ls:cd:cd -:pwd:exit:logout:date:* --help"
export HISTSIZE=32768
export HISTFILESIZE=$HISTSIZE
export HISTCONTROL=ignoredups
# Append to the Bash history file, rather than overwriting it
shopt -s histappend

# Prefer US English and use UTF-8
export LANG="en_US"
export LC_ALL="en_US.UTF-8"

# Always enable colored `grep` output
export GREP_OPTIONS="--color=auto"

[[ -s `brew --prefix`/etc/autojump.sh ]] && source `brew --prefix`/etc/autojump.sh

# Show list of matches immediately instead of double-tabbing
bind "set show-all-if-ambiguous On"

# Case-insensitive globbing (used in pathname expansion)
shopt -s nocaseglob

# Autocorrect typos in path names when using `cd`
shopt -s cdspell

# Enable some Bash 4 features when possible:
# * `autocd`, e.g. `**/qux` will enter `./foo/bar/baz/qux`
# * Recursive globbing, e.g. `echo **/*.txt`
for option in autocd globstar; do
    shopt -s "$option" 2> /dev/null
done
unset option
