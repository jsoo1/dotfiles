# Git
abbr --add -- gb 'git branch'
abbr --add -- gst 'git status'
abbr --add -- gf 'git fetch'
abbr --add -- ga 'git add'
abbr --add -- gc 'git commit'
abbr --add -- glg 'git log'
abbr --add -- grb 'git rebase'
abbr --add -- gl 'git pull'
abbr --add -- gp 'git push'
abbr --add -- gd 'git diff'
abbr --add -- gm 'git merge'
abbr --add -- gco 'git checkout'
function gitpurge -a ref
    git branch --merged | rg -v "\*" | rg -v "$ref" | xargs -n 1 git branch -d
end
alias vim nvim
# System Utils
abbr --add -- ls "exa"
abbr --add -- ll "exa -l"
abbr --add -- lsa "exa -la"
abbr --add -- lsah "exa -la"
abbr --add -- tree "exa -Ta"
abbr --add -- psg 'ps -e --format pid,command | rg -i'
abbr --add -- rest "loginctl suspend"
abbr --add -- apropos "man -k"

function shep
    herd -s $MY_SHEP_SOCK $argv
end

# Skim
function sk
    TERM=xterm-256color command sk --color=bw $argv
end

# Emacs
function em
    emacsclient -nw --socket-name term $argv
end
abbr --add -- e emacsclient -nw --socket-name term $argv

# Tmux
abbr --add -- tmux "tmux new-session -A -s (basename (pwd) | tr '.' '-') -n emacs"
function tm
    env tmux new-session -A -s (basename (pwd) | tr '.' '-') -n emacs
end
abbr --add -- tma "tmux attach -t"
abbr --add -- tml "tmux list-sessions"

# Lynx
abbr --add -- lynx lynx -cfg=~/.config/lynx/lynx.cfg
function google -a query
    lynx -cfg=~/.config/lynx/lynx.cfg "www.google.com/search?q='"$query"'"
end

function pursuit -a query
    lynx -cfg=~/.config/lynx/lynx.cfg "pursuit.purescript.org/search?q="$query
end

function pidof -d 'select a pid via fzy'
    ps waux | sk | sed -E 's/[[:space:]]+/ /g' | cut -d ' ' -f 2
end

complete -c pidof --no-files

function nth -a i -d 'Select the nth element of stdin, a file or files'
    if test (count $argv) -le 2
        head -n $i $argv[2] | tail -n 1
    else
        for file in $argv[2..-1]
            echo $file
            head -n $i "$file" | tail -n 1
            echo ''
        end
    end
end

function guix-signal -d 'Send a signal to guix processes'
    set -l sig
    set -l recsel recsel -t ChildProcess -j Session
    argparse 's/sig=' 'e/expr=' -- $argv
    or return 1
    if test "$_flag_s" = ""
        echo "--sig flag is required"
        return 1
    else
        set sig "$_flag_s"
    end
    if test "$_flag_e" != ""
        set -a recsel -e "\"$_flag_e\""
    end
    guix processes -f normalized \
        | eval $recsel \
        | recfmt -- ' -{{PID}} -{{Session.PID}}' \
        | xargs sudo kill -"$sig"
end
