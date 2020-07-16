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
function gitpurge -a ref
    git branch --merged | rg -v "\*" | rg -v "$ref" | xargs -n 1 git branch -d
end

# System Utils
abbr --add -U -- ls "exa"
abbr --add -U -- ll "exa -l"
abbr --add -U -- lsa "exa -la"
abbr --add -U -- lsah "exa -la"
abbr --add -U -- tree "exa -T"
abbr --add -U -- psg 'ps -e -o pid,command | rg -i'
abbr --add -U -- rest "pmset sleepnow"

# K8s
abbr --add -g k kubectl

# Emacs
function em -d "Start an emacsclient -t" -a file
    emacsclient -t --socket-name=term $file
end
abbr ed "emacs --bg-daemon=term"
abbr ec  "emacs --bg-daemon=frame"

# Tmux
abbr tma "tmux attach -t "
abbr ta "tmux attach -t"
abbr tml "tmux list-sessions"
abbr tmux "tmux new-session -A -n 'emacs' -s (basename (pwd))"

# Docker
abbr dockerpurge 'docker rmi (docker images -a --filter=dangling=true -q)'

# VPN
function curr_vpn -d 'which is the current vpn'
    launchctl list | rg -o '(pano|vetpro)\.de-tunnel' | sed 's/\.de-tunnel//' | sort -u
end

function vpin -d 'Get on a vpn' -a vpn
    set -l curr (curr_vpn)
    if test -z "$vpn"
        echo 'provide a vpn' && false
    else
        if test -z $curr
            launchctl bootstrap gui/(id -u) /Users/john/Library/LaunchAgents/com.$vpn.de-tunnel.plist
        else if test $curr
            echo "already on $curr"
        end
    end
end

complete -c vpin -a 'vetpro' -d 'get on vetpro' --no-files
complete -c vpin -a 'pano' -d 'get on gateway 2' --no-files

function vpout -d 'Get off the vpn' -a vpn
    set -l curr (curr_vpn)
    if test -n "$vpn"
        launchctl bootout gui/(id -u)/com."$vpn".de-tunnel
    else if test '' != "$curr"
        launchctl bootout gui/(id -u)/com.(curr_vpn).de-tunnel
    end
end

complete -c vpout --no-files

function tl -d 'select tldr from fzy'
    tldr (tldr --list | tr ', ' '\n ' | fzy | tr -d ' ')
end

complete -c tldr --no-files

function pidof -d 'select a pid via fzy'
    ps waux | fzy | sed -E 's/[[:space:]]+/ /g' | cut -d ' ' -f 2
end

complete -c pidof --no-files
