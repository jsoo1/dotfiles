set -gx PATH $PATH /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin /usr/games /usr/local/games /snap/bin /home/john/.cargo/bin /home/john/.local/bin /home/john/.nix-profile/bin

# GPG stuff
set -xg GPGKEY 69C2CD1B
set -xg GPG2KEY 802CD0C2

# Emacsclient for EDITOR
set -xg EDITOR 'emacsclient -t'
set -xg ALTERNATE_EDITOR 'vim'

# Fuzzy Finder
set -x FZF_DEFAULT_COMMAND 'rg --files --hidden --ignore .git --smartcase --glob "!.git/*"'

# vi mode
fish_vi_key_bindings

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
if test -e /home/john/.config/fish/aliases.fish
  source /home/john/.config/fish/aliases.fish
end

# :( no fzf defaults in fish
# if test -e ~/.fzf.zsh
#   source ~/.fzf.zsh
# end

# pretty vi mode
function fish_mode_prompt; end
function fish_mode_prompt --description 'Displays the current mode'
  # Do nothing if not in vi mode
  if set -q __fish_vi_mode
    switch $fish_bind_mode
      case default
        set_color --background BC6EC5 white
        echo '[N]'
      case insert
        set_color --bold --background green white
        echo '[I]'
      case visual
        set_color --bold --background magenta white
        echo '[V]'
    end
    set_color normal
    echo -n ' '
  end
end
