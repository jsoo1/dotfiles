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
abbr psg 'ps -eF | grep -i'
abbr rest "loginctl suspend"
abbr bat "upower -I (upower -e | grep BAT)"

# Emacs
abbr ed "emacs -q -l ~/dotfiles/emacs/init.el --bg-daemon=term"
abbr em "env TERM=xterm-24bits emacsclient -nw --socket-name term"
# TODO Figure out fish issues
abbr e "env TERM=xterm-24bits emacsclient -nw --socket-name term"

# Tmux
abbr tma "env TERM=xterm-24bits tmux attach -t"
abbr ta "env TERM=xterm-24bits tmux attach -t"
abbr tml "tmux list-sessions"
abbr tmux "env TERM=xterm-24bits tmux new-session -A -s (basename (pwd)) -n emacs"
abbr tm "env TERM=xterm-24bits tmux new-session -A -s (basename (pwd)) -n emacs"

# Lynx
abbr lynx = lynx -cfg=~/.config/lynx/lynx.cfg
function google
    lynx -cfg=~/.config/lynx/lynx.cfg www.google.com/search?q='"'argv[1]'"'
end
