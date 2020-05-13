set -gx PATH /Users/john/.local/bin $PATH /usr/local/anaconda3/bin /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin /Users/john/.cargo/bin /Users/john/Library/Python/3.6/bin

# Work related binaries
set -gx PATH $PATH /Users/john/projects/client-browser/.bin /Users/john/.cabal/bin
set -gx PKG_CONFIG_PATH $PKG_CONFIG_PATH '/usr/local/Cellar/libffi/3.2.1/lib/pkgconfig/'

# Emacsclient for EDITOR
set -xg EDITOR 'emacsclient -t --socket-name=term'
set -xg ALTERNATE_EDITOR 'vim'

# i3 Sensible Terminal
set -xg TERMINAL 'alacritty'

# Nix
if test -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
  fenv source '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
end

set -gx NIX_PATH $HOME/.nix-defexpr/channels $NIX_PATH

# fish cwd color
set -x fish_color_cwd yellow

# no greeting, plz
set fish_greeting ""

# solarized
# if test -e ~/.config/fish/colors.fish
#     source ~/.config/fish/colors.fish
# end

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
