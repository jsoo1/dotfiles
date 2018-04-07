# set -gx PATH $PATH /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin /usr/games /usr/local/games /home/john/.cargo/bin /home/john/.local/bin /home/john/.nix-profile/bin
if test -e ~/.guix-profile/bin
    set -gx PATH $PATH ~/.guix-profile/bin
end

if test -e ~/.config/fish/fish_prompt.fish
    source ~/.config/fish/fish_prompt.fish
end

# GPG stuff
set -xg GPGKEY 69C2CD1B
set -xg GPG2KEY 802CD0C2

# Emacsclient for EDITOR
set -xg EDITOR 'env TERM=xterm-24bits /Applications/Emacs.app/Contents/MacOS/bin/emacsclient -t --socket-name=base'
set -xg ALTERNATE_EDITOR 'vim'

# i3 Sensible Terminal
set -xg TERMINAL 'alacritty'

# Fuzzy Finder
set -x FZF_DEFAULT_OPTS '
  --color=bg+:#073642,spinner:#719e07,hl:#586e75
  --color=fg:#839496,header:#586e75,info:#cb4b16,pointer:#719e07
  --color=marker:#719e07,fg+:#839496,prompt:#719e07,hl+:#719e07
'
set -x FZF_DEFAULT_COMMAND 'rg --files --hidden --ignore .git --smartcase --glob "!.git/*"'

# fish cwd color
set -x fish_color_cwd magenta

# vi mode
fish_vi_key_bindings

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
function fish_title
  true
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
function fish_mode_prompt --description 'Displays the current mode'
  # Do nothing if not in vi mode
  if set -q __fish_vi_mode
    switch $fish_bind_mode
      case default
        set_color --background BC6EC5 white
        echo ''
      case insert
        set_color --bold --background green white
        echo ''
      case visual
        set_color --bold --background magenta white
        echo ''
    end
    set_color normal
    echo -n ' '
  end
end

# python direnv package
if test "Darwin" = (uname)
    eval (direnv hook fish)
end

eval (opam env)
