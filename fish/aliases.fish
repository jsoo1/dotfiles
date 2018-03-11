# Git
abbr gb 'git branch'
abbr gst 'git status'
abbr gf 'git fetch'
abbr ga 'git add'
abbr gc 'git commit'
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
abbr rest "systemctl suspend"
abbr logout "gnome-session-quit --logout --no-prompt"
abbr upgrade! "sudo apt-get update; sudo apt-get -y upgrade"
abbr install! "sudo apt-get update; sudo apt-get install"

# Emacs
abbr em "emacsclient -t --socket-name=term"

# Systemctl
abbr ctl "systemctl"
abbr ctlu "systemctl --user"
abbr ctllint "systemd-analyze verify"

# Email
abbr mindex "mu index --maildir ~/.mail"

# Tmux
abbr tma "tmux attach -t"
abbr tml "tmux list-sessions"

