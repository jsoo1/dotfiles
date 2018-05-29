# Git
abbr gb 'git branch'
abbr gbd 'git branch -D'
abbr gst 'git status'
abbr gf 'git fetch'
abbr ga 'git add'
abbr gc 'git commit'
abbr gcb 'git checkout -b'
abbr glg 'git log'
abbr grb 'git rebase'
abbr gl 'git pull'
abbr gp 'git push'
abbr gd 'git diff'
abbr gm 'git merge'
abbr gco 'git checkout'
alias gitpurge 'git branch --merged | grep -v "\*" | grep -v "master" | xargs -n 1 git branch -d'

# System Utils
abbr lsa "ls -lsa"
abbr lsah "ls -lsah"
abbr rmi "rm -i"
abbr psg "ps aux | rg -i"
abbr rest "loginctl suspend"
abbr logout "gnome-session-quit --logout --no-prompt"
abbr upgrade! "sudo apt-get update; sudo apt-get -y upgrade"
abbr install! "sudo apt-get update; sudo apt-get install"

# Emacs
abbr ed "emacs --daemon=term"
abbr em "emacsclient -t --socket-name=term"

# Systemctl
abbr ctl "systemctl"
abbr ctlu "systemctl --user"
abbr ctllint "systemd-analyze verify"

# Email
abbr mindex "mu index --maildir ~/.mail"

# Tmux
function tma -d "Select a tmux session with fuzzy search"
    env TERM=xterm-24bits tmux attach -t (tmux list-sessions | fzf --height=15% | cut -f 1 -d :)
end
abbr ta "env TERM=xterm-24bits tmux attach -t"
abbr tml "tmux list-sessions"
abbr tmux "tmux new-session -A -s (basename (pwd)) -n emacs"
