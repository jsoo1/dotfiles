export ZSH=/home/john/.oh-my-zsh

ZSH_THEME="alanpeabody"

plugins=(git dirhistory emoji git-prompt gitfast systemd vi-mode zsh-autosuggestions)

export PATH="$PATH/usr/local/heroku/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin:/home/john/.cargo/bin:/home/john/.local/bin"

# GPG stuff
export GPGKEY=69C2CD1B
export GPG2KEY=802CD0C2

# Emacsclient for EDITOR
export EDITOR='emacsclient -t'

source $ZSH/oh-my-zsh.sh

# Fuzzy Finder
export FZF_DEFAULT_COMMAND='rg --files --hidden --ignore .git --smartcase --glob "!.git/*"'

if [ -f ~/.zsh_aliases ]; then
  . ~/.zsh_aliases
fi

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
