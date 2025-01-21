# Emacs
function em
    emacsclient -nw --socket-name term $argv
end

# Tmux
function tm
    env tmux new-session -A -s (basename (pwd) | tr '.' '-') -n emacs
end

# Misc
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
