source_if_possible()
{
    if [[ -r $1 ]]; then
        source $1
    fi
}

source_if_possible "${HOME}/.zprofile_local"

# Prevent duplicate entries in `PATH`. zsh has already implicitly tied (typeset
# -T) `path` and `PATH` together.
typeset -U path
export PATH
