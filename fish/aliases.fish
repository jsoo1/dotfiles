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
abbr ls "exa"
abbr ll "exa -l"
abbr lsa "exa -la"
abbr lsah "exa -la"
abbr tree "exa -T"
abbr rmi "rm -i"
abbr psg "ps aux | rg -i"
abbr dfh "df -H"
abbr rest "pmset sleepnow"

# Emacs
function em -d "Start an emacsclient -t" -a file
    env TERM=xterm-24bits emacsclient -t --socket-name=term $file
end
abbr ed "emacs --daemon=term"
abbr ec  "emacs --daemon=frame"

# Tmux
function tma -d "Select a tmux session with fuzzy search"
    env TERM=xterm-24bits tmux attach -t (tmux list-sessions | fzf --height=15% | cut -f 1 -d :)
end
abbr ta "env TERM=xterm-24bits tmux attach -t"
abbr tml "tmux list-sessions"
abbr tmux "env TERM=xterm-24bits tmux new-session -A -n 'emacs' -s (basename (pwd))"

# Docker
abbr dockerpurge 'docker rmi (docker images -a --filter=dangling=true -q)'

# VPN
function vpin -d 'Get on a vpn' -a vpn
    set -l curr_vpn (launchctl list | rg -o 'pano|vetpro')
    if test -z $curr_vpn
        launchctl bootstrap gui/(id -u) /Users/john/Library/LaunchAgents/com.$vpn.de-tunnel.plist
    else if test $curr_vpn != $vpn
        launchctl bootout gui/(id -u)/com.$curr_vpn.de-tunnel
        launchctl bootstrap gui/(id -u) /Users/john/Library/LaunchAgents/com.$vpn.de-tunnel.plist
    end
end

complete -c vpin -a 'vetpro' -d 'get on vetpro' --no-files
complete -c vpin -a 'pano' -d 'get on gateway 2' --no-files

function vpout -d 'Get off the vpn' -a vpn
    set -l curr_vpn (launchctl list | rg --color=never -o 'pano|vetpro')
    if test $vpn
        launchctl bootout gui/(id -u)/com.$vpn.de-tunnel
    else if test $curr_vpn
        launchctl bootout gui/(id -u)/com.$curr_vpn.de-tunnel
    end
end

complete -c vpout --no-files

function tl -d 'select tldr from fzf'
    tldr (tldr --list | tr ', ' '\n ' | fzf | tr -d ' ')
end

complete -c tldr --no-files

function pidof -d 'select a pid via fzf'
    ps waux | fzf | sed -E 's/[[:space:]]+/ /g' | cut -d ' ' -f 2
end

complete -c pidof --no-files
