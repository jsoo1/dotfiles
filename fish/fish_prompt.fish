# name: taktoa
# by taktoa (Remy Goldschmidt) <taktoa@gmail.com>
# License: public domain
# Modified heavily :)

function _remote_hostname
  echo (whoami)
  if test -n "$SSH_CONNECTION"
    echo " (ssh) "
  end
end

# Updated prompt colors
function fish_prompt
  set -l green (set_color green)
  set -l blue (set_color blue)
  set -l normal (set_color normal)

  set -l arrow "λ"
  set -l cwd (set_color $fish_color_cwd)(prompt_pwd)

  if test -n "$VIRTUAL_ENV"
      set venv (basename "$VIRTUAL_ENV")
      set virtualenv "($venv)"
  else if test -n "$CONDA_DEFAULT_ENV"
      set virtualenv "($CONDA_DEFAULT_ENV)"
  else
      set virtualenv ""
  end

  if test -n "$GUIX_ENVIRONMENT"
      set genv " [env]"
  else
      set genv ""
  end

  echo -n -s (_remote_hostname) $green $genv$virtualenv ' ' $cwd $blue (fish_vcs_prompt) $normal ' ' $arrow ' '
end

function fish_right_prompt -d "Show the time as the right prompt"
    set_color normal
    date "+%a %I:%M:%S %p"
end
