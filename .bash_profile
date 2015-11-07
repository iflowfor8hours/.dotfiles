# Bash reads ~/.bash_profile for login shells (when you log in to a computer
# with username and password, via ssh, or similar). Bash reads ~/.bashrc for
# interactive non-login shells (anytime you start a new instance of Bash after
# logging in). Terminal.app (and possibly other GUI terminal emulators) always
# start a login shell, although you are already logged in to the computer.
# 
# Typically, ~/.bash_profile does any login-specific stuff and then sources
# ~/.bashrc.
# 
# See also
# * http://www.gnu.org/software/bash/manual/bashref.html#Bash-Startup-Files
# * http://www.joshstaiger.org/archives/2005/07/bash_profile_vs.html

if [ -r "${HOME}/.bashrc" ]; then
    source "${HOME}/.bashrc"
fi
