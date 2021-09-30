alias tmux='tmux new-session -A -s $(basename $PWD | tr '.' '-') -n emacs'
alias em='emacsclient -t'
alias lsa='ls -lsa'
alias vi=nvim
eval "$(direnv hook zsh)"
