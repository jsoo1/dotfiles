# Common Hosts
export PI_AWS="ec2-54-186-253-144.us-west-2.compute.amazonaws.com"
export PI_SERVER="192.168.1.92"

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
abbr psg "ps aux | grep -i"
abbr rest "systemctl suspend"
abbr off "systemctl poweroff"
abbr logout "gnome-session-quit --logout --no-prompt"
abbr upgrade! "sudo apt-get update; sudo apt-get -y upgrade"
abbr install! "sudo apt-get update; sudo apt-get install"

# Emacs
abbr emc "emacsclient -c --socket-name=frame &"
abbr em "emacsclient -t --socket-name=term"

# Systemctl
abbr ctl "systemctl"
abbr ctlu "systemctl --user"
abbr ctllint "systemd-analyze verify"
abbr ctlulint "systemd-analyze --user verify"

# Email
abbr mindex "mu index --maildir ~/.mail"

# Tmux
abbr tma "tmux attach -t"
abbr tml "tmux list-sessions"

# i3
abbr swapcaps "setxkbmap -layout us -option ctrl:swapcaps"
