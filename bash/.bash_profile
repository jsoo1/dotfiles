export PATH="/home/john/.local/bin:$PATH"
export EDITOR="TERM=xterm-24bits emacsclient -nw --socket-name term"
export GIT_EXEC_PATH="/home/john/.guix-profile/libexec/git-core"

# Honor per-interactive-shell startup file
if [ -f ~/.bashrc ]; then . ~/.bashrc; fi

