# name: taktoa
# by taktoa (Remy Goldschmidt) <taktoa@gmail.com>
# License: public domain
# Modified heavily :)

function _git_branch_name
  echo (command git symbolic-ref HEAD ^/dev/null | sed -e 's|^refs/heads/||')
end

function _git_status_symbol
  set -l git_status (git status --porcelain ^/dev/null)
  if test -n "$git_status"
    if git status --porcelain ^/dev/null | grep '^.[^ ]' >/dev/null
      echo '*' # dirty
    else
      echo '#' # all staged
    end
  else
    echo    '' # clean
  end
end

function _remote_hostname
  echo (whoami)
  if test -n "$SSH_CONNECTION"
    echo " (ssh)"
  end
end

# Updated prompt colors
function fish_prompt
  set -l green (set_color green)
  set -l blue (set_color blue)
  set -l normal (set_color normal)

  set -l arrow "λ"
  set -l cwd (set_color $fish_color_cwd)(prompt_pwd)
  set -l git_status (_git_status_symbol)(_git_branch_name)

  if test -n "$git_status"
    set git_status " $git_status"
  end

  if test -n "$VIRTUAL_ENV"
      set venv (basename "$VIRTUAL_ENV")
      set virtualenv "($venv)"
  else if test -n "$CONDA_DEFAULT_ENV"
      set virtualenv "($CONDA_DEFAULT_ENV)"
  else
      set virtualenv ""
  end


  echo -n -s (_remote_hostname) ' ' $green $virtualenv ' ' $cwd $blue $git_status $normal ' ' $arrow ' '
end

function fish_right_prompt -d "Show the time as the right prompt"
    set_color black
    date "+%a %I:%M:%S %p"
    set_color normal
end
