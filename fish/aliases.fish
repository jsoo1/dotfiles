# Git
abbr gb 'git branch'
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
abbr dfh "df -H"
abbr rest "pmset sleepnow"

# Emacs
abbr em "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -t --socket-name=term"
abbr ed "/Applications/Emacs.app/Contents/MacOS/Emacs --daemon=term"
abbr e "/Applications/Emacs.app/Contents/MacOS/Emacs"

# Tmux
abbr tma "tmux attach -t"
abbr tml "tmux list-sessions"

