# Git
abbr --add -U -- gb 'git branch'
abbr --add -U -- gst 'git status'
abbr --add -U -- gf 'git fetch'
abbr --add -U -- ga 'git add'
abbr --add -U -- gc 'git commit'
abbr --add -U -- glg 'git log'
abbr --add -U -- grb 'git rebase'
abbr --add -U -- gl 'git pull'
abbr --add -U -- gp 'git push'
abbr --add -U -- gd 'git diff'
abbr --add -U -- gm 'git merge'
abbr --add -U -- gco 'git checkout'
alias gitpurge 'git branch --merged | grep -v "\*" | grep -v "master" | xargs -n 1 git branch -d'

# System Utils
abbr --add -U -- lsa "ls -lsa"
abbr --add -U -- lsah "ls -lsah"
abbr --add -U -- psg 'ps -eF | grep -i'
abbr --add -U -- rest "loginctl suspend"
abbr --add -U -- bat "upower -I (upower -e | grep BAT)"

# Emacs
abbr --add -U -- ed "emacs -q -l ~/dotfiles/emacs/init.el --bg-daemon=term"
abbr --add -U -- em "env TERM=xterm-24bits emacsclient -nw --socket-name term"
# TODO Figure out fish issues
# abbr --add -U -- e "env TERM=xterm-24bits emacsclient -nw --socket-name term"
function em
    env TERM=xterm-24bits emacsclient -nw --socket-name term
end

# Tmux
abbr --add -U -- tma "env TERM=xterm-24bits tmux attach -t"
abbr --add -U -- ta "env TERM=xterm-24bits tmux attach -t"
abbr --add -U -- tml "tmux list-sessions"
abbr --add -U -- tmux "env TERM=xterm-24bits tmux new-session -A -s (basename (pwd)) -n emacs"
abbr --add -U -- tm "env TERM=xterm-24bits tmux new-session -A -s (basename (pwd)) -n emacs"

# Lynx
abbr lynx = lynx -cfg=~/.config/lynx/lynx.cfg
function google -a query
    lynx -cfg=~/.config/lynx/lynx.cfg "www.google.com/search?q='"$query"'"
end

function pursuit -a query
    lynx -cfg=~/.config/lynx/lynx.cfg "pursuit.purescript.org/search?q="$query
end
