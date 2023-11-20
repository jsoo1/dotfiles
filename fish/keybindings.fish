# vi mode
fish_hybrid_key_bindings

# ---------- SKIM -----------
bind \cr __skim_history
bind -M insert \cr __skim_history

bind \ct __skim_files
bind -M insert \ct __skim_files

# --------- Git repos ---------
bind \co __skim_git_repos
bind -M insert \co __skim_git_repos

# --------- Lpass ---------
bind \el 'skim_lpass | xsel -ib'
bind -M insert \el 'skim_lpass | xsel -ib'

# MIT License for __fzf_parse_commandline and __fzf_get_dir
# The MIT License (MIT)

# Copyright (c) 2016 Junegunn Choi

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

# Thanks to https://github.com/junegunn/fzf/blob/master/shell/key-bindings.fish
function __fzf_parse_commandline -d 'Parse the current command line token and return split of existing filepath and rest of token'
    # eval is used to do shell expansion on paths
    set -l commandline (eval "printf '%s' "(commandline -t))

    if [ -z $commandline ]
        # Default to current directory with no --query
        set dir '.'
        set fzf_query ''
    else
        set dir (__fzf_get_dir $commandline)

        if [ "$dir" = "." -a (string sub -l 1 $commandline) != '.' ]
            # if $dir is "." but commandline is not a relative path, this means no file path found
            set fzf_query $commandline
        else
            # Also remove trailing slash after dir, to "split" input properly
            set fzf_query (string replace -r "^$dir/?" '' "$commandline")
        end
    end

    echo $dir
    echo $fzf_query
end

# Thanks to https://github.com/junegunn/fzf/blob/master/shell/key-bindings.fish
function __fzf_get_dir -d 'Find the longest existing filepath from input string'
    set dir $argv

    # Strip all trailing slashes. Ignore if $dir is root dir (/)
    if [ (string length $dir) -gt 1 ]
        set dir (string replace -r '/*$' '' $dir)
    end

    # Iteratively check if dir exists and strip tail end of path
    while [ ! -d "$dir" ]
        # If path is absolute, this can keep going until ends up at /
        # If path is relative, this can keep going until entire input is consumed, dirname returns "."
        set dir (dirname "$dir")
    end

    echo $dir
end

function __skim_files -d "List files and folders"
    set -l commandline (__fzf_parse_commandline)

    begin
        fd '.*' '.' --hidden -E '.git*' | sk | while read -l r; set result $result $r; end
    end
    if [ -z "$result" ]
        commandline -f repaint
        return
    else
        # Remove last token from commandline.
        commandline -t ""
    end
    for i in $result
        commandline -it -- (string escape $i)
        commandline -it -- ' '
    end
    commandline -f repaint
end

function __skim_history -d "Find in history"
    begin
        history | sk | read -l result
        and commandline -- $result
    end
    commandline -f repaint
end

function __ls_git_repos -d "List git repos"
    fd '\.git' '/' -t d -H -I \
        -E '\.gitlab' \
        -E '\.github' \
        -E '\.cache' \
        -E '\.tmux' \
        -E '\.cargo' \
        -E /gnu/store \
        -E '\.git-credential-cache' \
        -E IndexedDB \
        -E '\.spago' \
        -E '\.emacs\.d' \
        -E '\.racket' \
        -E '/nix/store' \
        -E '/tmp' \
        -E '\.local' \
        -E 'dist-newstyle' \
        -E '\.opam' \
        -E '\/s3' \
        -x echo '{//}' ';'
end

function __skim_git_repos -d "Find a git repo"
    begin
        __ls_git_repos | sk | read -l result
        and commandline -it -- "tmux new-session -A -s (basename $result | tr '.' '-') -c $result -n emacs 'exec $EDITOR $result'"
    end
end

function skim_docker_images -d "Find a docker image"
    begin
        docker images \
        | awk '{ print $1 ":" $2 " " $3 }' \
        | rg -v 'REPOSITORY:TAG' \
        | column -t \
        | sk \
        | awk '{ print $1 }' \
        | read -l result
        and commandline -it -- $result
    end
end

function skim_docker_containers -d "Find a docker container"
    begin
        docker ps -a \
        | awk '{ print $1 ":" $2 " " $3 }' \
        | rg -v '^CONTAINER:ID' \
        | sk \
        | awk '{ print $1 }' \
        | read -l result
        and commandline -it -- $result
    end
end

function skim_lpass -d "Get a password"
    if test (lpass status) = "Not logged in."; lpass login jsoo1@asu.edu; end
    and lpass ls --color=never \
    | sk \
    | read -l result
    and echo "$result" \
    | sed -E 's/^.*id: ([0-9]+)]$/\1/' \
    | xargs lpass show --color=never --password
    commandline
end
