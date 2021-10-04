# ----- Aliases & Functions -----
alias tm='tmux new-session -A -s $(basename $PWD | tr '.' '-') emacs'
alias tml='command tmux list-sessions'
alias tma='command tmux attach-session -t'
alias em='emacsclient -t'
alias lsa='ls -lsa'
alias sk='TERM=xterm-256color command sk --color=bw'
alias vi=nvim
restart-nix-daemon () {
    sudo launchctl bootout system/org.nixos.nix-daemon \
        && sudo launchctl load /Library/LaunchDaemons/org.nixos.nix-daemon.plist
}

# ----- Keybindings -----
__skim_history () {
    LBUFFER="$LBUFFER$(history | sk | awk '{for (i=2; i<NF; i++) printf $i " "; print $NF}')"
}
zle -N skim_history __skim_history
__skim_files () {
    LBUFFER="$LBUFFER$(fd '.*' '.' --hidden -E '.git*' | sk)"
}
zle -N skim_files __skim_files
bindkey '^r' skim_history
bindkey '^t' skim_files

# ----- Direnv -----
eval "$(direnv hook zsh)"
