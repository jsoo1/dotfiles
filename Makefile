XDG_HOME ?= $(HOME)/.config

HOME_DIRS = \
	. \
	.emacs.d \
	.emacs.d/eshell \
	.gnupg \
	.local/share/applications \
	.tmux \
	.xmonad

XDG_HOME_DIRS = \
	alacritty \
	compton \
	dunst \
	fish \
	git \
	guix \
	lynx \
	shepherd \
	zathura

DIRS = \
	$(foreach dir,$(HOME_DIRS),$(HOME)/$(dir)) \
	$(foreach dir,$(XDG_HOME_DIRS),$(XDG_HOME)/$(dir))

SYMLINKS = \
	$(HOME)/.profile \
	$(HOME)/.emacs.d/init.el \
	$(HOME)/.emacs.d/feeds \
	$(HOME)/.emacs.d/eshell/alias \
	$(HOME)/.local/share/applications/defaults.list \
	$(XDG_HOME)/alacritty/alacritty.yml \
	$(HOME)/.bashrc \
	$(HOME)/.bash_profile \
	$(XDG_HOME)/compton/compton.conf \
	$(XDG_HOME)/chromium-flags.conf \
	$(XDG_HOME)/dunst/dunstrc \
	$(XDG_HOME)/fish/aliases.fish \
	$(XDG_HOME)/fish/colors.fish \
	$(XDG_HOME)/fish/config.fish \
	$(XDG_HOME)/fish/fish_prompt.fish \
	$(XDG_HOME)/fish/keybindings.fish \
	$(HOME)/.ghci \
	$(HOME)/.gnupg/gnupg.conf \
	$(HOME)/.gnus \
	$(HOME)/.guile \
	$(HOME)/.haskeline \
	$(XDG_HOME)/git/config \
	$(XDG_HOME)/guix/channels.scm \
	$(XDG_HOME)/lynx/lynx.cfg \
	$(HOME)/.psqlrc \
	$(HOME)/.inputrc \
	$(XDG_HOME)/shepherd/init.scm \
	$(HOME)/.tmux.conf \
	$(HOME)/.xsession \
	$(XDG_HOME)/zathura/zathurarc

ln = ln -s

# ----------- top level commands -------------
.PHONY: install
install: | $(SYMLINKS) ## Install $(SYMLINKS) to $HOME (default)

.PHONY: help
help:
	@grep -E '^[a-zA-Z_()$$. /-]+:.*?## .*$$' Makefile | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

# ------------ softlinks -------------
$(HOME)/.profile: | $(HOME) ## The profile for session initialization. Not posix compliant.
	$(ln) $(PWD)/.profile $@

$(HOME)/.emacs.d/init.el: | $(HOME)/.emacs.d ## Emacs initialization file
	$(ln) $(PWD)/emacs/init.el $@

$(HOME)/.emacs.d/feeds: | $(HOME)/.emacs.d ## subscriptions for elfeed
	$(ln) $(PWD)/rss $@

$(HOME)/.emacs.d/eshell/alias: | $(HOME)/.emacs.d/eshell
	$(ln) $(PWD)/emacs/eshell/alias $@

$(HOME)/.local/share/applications/defaults.list: | $(HOME)/.local/share/applications ## Default mime handlers
	$(ln) $(PWD)/xdg/defaults.list $@

$(XDG_HOME)/alacritty/alacritty.yml: | $(XDG_HOME)/alacritty ## Alacritty configuration
	$(ln) $(PWD)/alacritty/alacritty.yml $@

$(HOME)/.bashrc: | $(HOME) ## Bash configuration (per shell)
	$(ln) $(PWD)/bash/.bashrc $@

$(HOME)/.bash_profile: | $(HOME) ## Bash configuration (per session)
	$(ln) $(PWD)/bash/.bash_profile $@

$(XDG_HOME)/chromium-flags.conf: | $(XDG_HOME) ## Compton configuration
	$(ln) $(PWD)/chromium/chromium-flags.conf $@

$(XDG_HOME)/compton/compton.conf: | $(XDG_HOME)/compton ## Compton configuration
	$(ln) $(PWD)/compton/compton.conf $@

$(XDG_HOME)/dunst/dunstrc: | $(XDG_HOME)/dunst ## Dunst configuration
	$(ln) $(PWD)/dunst/dunstrc $@

$(XDG_HOME)/fish/aliases.fish: | $(XDG_HOME)/fish ## Fish aliases, abbreviations, and functions
	$(ln) $(PWD)/fish/aliases.fish $@

$(XDG_HOME)/fish/colors.fish: | $(XDG_HOME)/fish ## Fish color definitions
	$(ln) $(PWD)/fish/colors.fish $@

$(XDG_HOME)/fish/config.fish: | $(XDG_HOME)/fish ## Fish initialization
	$(ln) $(PWD)/fish/config.fish $@

$(XDG_HOME)/fish/fish_prompt.fish: | $(XDG_HOME)/fish ## Fish prompt
	$(ln) $(PWD)/fish/fish_prompt.fish $@

$(XDG_HOME)/fish/keybindings.fish: | $(XDG_HOME)/fish ## Fish keybindings
	$(ln) $(PWD)/fish/keybindings.fish $@

$(HOME)/.ghci: | $(HOME) ## ghci configuration
	$(ln) $(PWD)/ghci/.ghci $@

$(HOME)/.gnupg/gnupg.conf: | $(HOME)/.gnupg ## gnupg configuration
	$(ln) $(PWD)/gnupg/gnupg.conf $@

$(HOME)/.gnus: | $(HOME) ## Gnus configuration
	$(ln) $(PWD)/emacs/.gnus $@

$(HOME)/.guile: | $(HOME) ## Guile configuration
	$(ln) $(PWD)/guile/.guile $@

$(HOME)/.haskeline: | $(HOME) ## haskeline configuration (for ghci)
	$(ln) $(PWD)/ghci/.haskeline $@

$(XDG_HOME)/git/config: | $(XDG_HOME)/git ## Git configuration
	$(ln) $(PWD)/git/.gitconfig $@

$(XDG_HOME)/guix/channels.scm: | $(XDG_HOME)/guix ## Guix channel specification
	$(ln) $(PWD)/guix/channels.scm $@

$(XDG_HOME)/lynx/lynx.cfg: | $(XDG_HOME)/lynx ## Lynx configuration
	$(ln) $(PWD)/lynx/lynx.cfg $@

$(HOME)/.psqlrc: | $(HOME) ## psql configuration
	$(ln) $(PWD)/psql/.psqlrc $@

$(HOME)/.inputrc: | $(HOME) ## readline configuration
	$(ln) $(PWD)/readline/.inputrc $@

$(XDG_HOME)/shepherd/init.scm: | $(XDG_HOME)/shepherd ## User shepherd configuration and services
	$(ln) $(PWD)/shepherd/init.scm $@

$(HOME)/.tmux.conf: | $(HOME) ## Tmux configuration
	$(ln) $(PWD)/tmux/.tmux.conf $@

$(HOME)/.xsession: | $(HOME) ## Loaded by gdm and other DMs on login
	$(ln) $(PWD)/xmonad/.xsession $@

$(XDG_HOME)/zathura/zathurarc: | $(XDG_HOME)/zathura ## Zathura configuration
	$(ln) $(PWD)/zathura/zathurarc $@

$(DIRS): ## Make sure containing directories exist
	mkdir -p $@
