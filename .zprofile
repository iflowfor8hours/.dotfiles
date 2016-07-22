source_if_possible()
{
    if [[ -r $1 ]]; then
        source $1
    fi
}

# Source zplug's init script here to avoid changing PATH and FPATH the rc file
source "$HOME/.zplug/init.zsh"

source_if_possible "${HOME}/.local_zprofile"
