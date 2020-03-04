if test -z "$MY_SHEP_SOCK"
    rm "$HOME/var/run/shepherd/socket" \
    && shepherd --quiet --insecure \
        --socket="$HOME/var/run/shepherd/socket" \
        --config="$HOME/dotfiles/shepherd/init.scm" \
        --logfile="$HOME/var/log/shepherd.log" \
    && set -Ux MY_SHEP_SOCK "$HOME/var/run/shepherd/socket"
end

set -gx PATH $PATH /home/john/.cargo/bin /home/john/.local/bin

# Emacsclient for EDITOR
set -xg EDITOR 'emacsclient -t --socket-name=term'
set -xg ALTERNATE_EDITOR 'vim'

# Something weird happens without this
set -xg GIT_EXEC_PATH /run/current-system/profile/libexec/git-core

# Or this
set -xg GUILE_LOAD_PATH $GUILE_LOAD_PATH /home/john/.guix-profile/share/guile/site/2.2

# Fuzzy Finder
set -x FZF_DEFAULT_OPTS '
  --color=bg+:#073642,spinner:#719e07,hl:#586e75
  --color=fg:#839496,header:#586e75,info:#cb4b16,pointer:#719e07
  --color=marker:#719e07,fg+:#839496,prompt:#719e07,hl+:#719e07
'
set -x FZF_DEFAULT_COMMAND 'rg --files --hidden --ignore .git --smartcase --glob "!.git/*"'

# cargo
set -x CARGO_HOME "~/.cargo"

# fish cwd color
set -x fish_color_cwd yellow

# no greeting, plz
set fish_greeting ""

# solarized
if test -e ~/.config/fish/colors.fish
    source ~/.config/fish/colors.fish
end

# prompt :)
if test -e ~/.config/fish/fish_prompt.fish
    source ~/.config/fish/fish_prompt.fish
end

# keybindings
if test -e ~/.config/fish/keybindings.fish
    source ~/.config/fish/keybindings.fish
end

# fixes for emacs
# emacs ansi-term support
if test -n "$EMACS"
  set -x TERM eterm-color
end

# hopeful fix for no binding error messages
# See issue 1907:
# https://github.com/fish-shell/fish-shell/issues/1907
if test "$TERM" = "dumb"
  function fish_title; end
end

# this function may be required
# function fish_title
#   true
# end

# aliases
if test -e ~/.config/fish/aliases.fish
  source ~/.config/fish/aliases.fish
end

# private
if test -e ~/.config/fish/private.fish
    source ~/.config/fish/private.fish
end

# pretty vi mode
function fish_mode_prompt; end

set fish_cursor_default     block
set fish_cursor_insert      block
set fish_cursor_replace_one underscore
set fish_cursor_visual      block

# python direnv package
if test "Darwin" = (uname)
    eval (direnv hook fish)
end

# eval (opam env)
