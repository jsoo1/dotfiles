set -gx PATH $PATH /home/john/.cargo/bin /home/john/.local/bin 

# Emacsclient for EDITOR
set -xg EDITOR 'env TERM=xterm-24bits emacsclient -t --socket-name=term'
set -xg ALTERNATE_EDITOR 'vim'

# Fuzzy Finder
set -x FZF_DEFAULT_OPTS '
  --color=bg+:#073642,spinner:#719e07,hl:#586e75
  --color=fg:#839496,header:#586e75,info:#cb4b16,pointer:#719e07
  --color=marker:#719e07,fg+:#839496,prompt:#719e07,hl+:#719e07
'
set -x FZF_DEFAULT_COMMAND 'rg --files --hidden --ignore .git --smartcase --glob "!.git/*"'

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
