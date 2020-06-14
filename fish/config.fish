set -Ux MY_SHEP_SOCK "$HOME/var/run/shepherd/socket"

source "$HOME/.profile";

set -gx PATH /home/john/.cargo/bin /home/john/.local/bin $PATH

# Emacsclient for EDITOR
set -xg EDITOR 'emacsclient -t --socket-name=term'
set -xg ALTERNATE_EDITOR 'vim'

set -xg PAGER 'less'

# Icecat for BROWSER
set -xg BROWSER /home/john/.guix-profile/bin/icecat

# Something weird happens without this
set -xg GIT_EXEC_PATH /run/current-system/profile/libexec/git-core

# Or this
set -xg GUILE_LOAD_PATH $GUILE_LOAD_PATH /home/john/.guix-profile/share/guile/site/3.0

# cargo
set -x CARGO_HOME "$HOME/.cargo"

# fish cwd color
set -x fish_color_cwd yellow

# no greeting, plz
set fish_greeting ""

# solarized
# test -e ~/.config/fish/colors.fish;
# and source ~/.config/fish/colors.fish

# prompt :)
test -e ~/.config/fish/fish_prompt.fish;
and source ~/.config/fish/fish_prompt.fish

# keybindings
test -e ~/.config/fish/keybindings.fish;
and source ~/.config/fish/keybindings.fish

# fixes for emacs
# emacs ansi-term support
test -n "$INSIDE_EMACS"; and set -x TERM eterm-color

# hopeful fix for no binding error messages
# See issue 1907:
# https://github.com/fish-shell/fish-shell/issues/1907
test -n "$INSIDE_EMACS"; and function fish_title; end

# aliases
test -e ~/.config/fish/aliases.fish;
and source ~/.config/fish/aliases.fish

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

# eval (opam env)

if test (tty) = /dev/tty1 && status is-login
    xinit ~/.xsession -- /run/setuid-programs/*startx vt1
    loginctl terminate-session (loginctl list-sessions | gawk '/tty1/ { print $1 }')
end
