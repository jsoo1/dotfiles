# Bash initialization for interactive non-login shells and
# for remote shells (info "(bash) Bash Startup Files").

# Export 'SHELL' to child processes.  Programs such as 'screen'
# honor it and otherwise use /bin/sh.
export SHELL

if [[ $- != *i* ]]
then
    # We are being invoked from a non-interactive shell.  If this
    # is an SSH session (as in "ssh host command"), source
    # /etc/profile so we get PATH and other essential variables.
    [[ -n "$SSH_CLIENT" ]] && source /etc/profile

    # Don't do anything else.
    return
fi

# Adjust the prompt depending on whether we're in 'guix environment'.
if [ -n "$GUIX_ENVIRONMENT" ]
then
    PS1='\u@\h \w [env] λ '
else
    PS1='\u@\h \w λ '
fi

alias ls='ls -p --color'
alias ll='ls -l'
alias grep='grep --color'
alias clear="printf '\033c'"
alias lsa="ls -lsa"
alias lsah="ls -lsah"

alias rest="loginctl suspend"
alias psg="ps aux"
alias lynx="lynx -cfg=~/.config/lynx/lynx.cfg"
function google () {
    lynx -cfg=~/.config/lynx/lynx.cfg www.google.com/search?q='"'"$1"'"'
}
alias ed="emacs -q -l ~/dotfiles/emacs/init.el --bg-daemon=term"
alias ec="emacs --bg-daemon=frame"
alias em="TERM=xterm-24bits emacsclient -nw --socket-name term"
alias tm='TERM=xterm-24bits tmux new-session -A -s $(basename $(pwd))'
alias tml="tmux list-sessions"
alias tma="TERM=xterm-24bits tmux attach-session -t"
alias bat='upower -i $(upower -e | grep BAT)'
alias psg='ps aux | grep -i'
