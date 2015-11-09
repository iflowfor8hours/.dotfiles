export EDITOR="emacs"

# Prefer US English and use UTF-8
export LANG="en_US"
export LC_ALL="en_US.UTF-8"

# Always enable colored `grep` output
export GREP_OPTIONS="--color=auto"

# Get a sorted list of subfolder sizes, as long as `sort` supports the `-h` option
alias dirsize="du --human-readable --max-depth=1 --exclude='./.*' | sort --human-numeric-sort --reverse"
alias dirsizeall="du --human-readable --max-depth=1 | sort --human-numeric-sort --reverse"
