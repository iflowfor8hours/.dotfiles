export EDITOR="emacs"

# Always enable colored `grep` output
export GREP_OPTIONS="--color=auto"

# Get a sorted list of subfolder sizes, as long as `sort` supports the `-h` option
alias dirsize="du --human-readable --max-depth=1 --exclude='./.*' | sort --human-numeric-sort --reverse"
alias dirsizeall="du --human-readable --max-depth=1 | sort --human-numeric-sort --reverse"

# Complement `whoami`
alias whereami="uname -n"
