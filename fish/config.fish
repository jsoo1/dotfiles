# fish cwd color
set -x fish_color_cwd yellow

# no greeting, plz
set fish_greeting ""

# fixes for emacs
# emacs ansi-term support
test -n "$INSIDE_EMACS"; and set -x TERM eterm-color

# hopeful fix for no binding error messages
# See issue 1907:
# https://github.com/fish-shell/fish-shell/issues/1907
test -n "$INSIDE_EMACS"; and function fish_title; end

test -e ~/.config/fish/private.fish;
and source ~/.config/fish/private.fish

# pretty vi mode
function fish_mode_prompt; end

set fish_cursor_default     block
set fish_cursor_insert      block
set fish_cursor_replace_one underscore
set fish_cursor_visual      block

# python direnv package
eval (direnv hook fish)

# opam
source /home/john/.opam/opam-init/init.fish > /dev/null 2> /dev/null;
or true
