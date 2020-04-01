XDG_HOME ?= $(HOME)/.config
MODPROBE = /run/modprobe.d

DIRS = \
	$(HOME) \
	$(HOME)/.emacs.d \
	$(HOME)/.gnupg \
	$(XDG_HOME)/alacritty \
	$(XDG_HOME)/compton \
	$(XDG_HOME)/dunst \
	$(XDG_HOME)/fish \
	$(XDG_HOME)/git \
	$(XDG_HOME)/guix \
	$(XDG_HOME)/lynx \
	$(XDG_HOME)/shepherd \
	$(HOME)/.tmux \
	$(XDG_HOME)/xmobar \
	$(HOME)/.xmonad \
	$(XDG_HOME)/zathura

SOFTLINKS = \
	$(HOME)/.profile \
	$(HOME)/.emacs.d/init.el \
	$(XDG_HOME)/alacritty/alacritty.yml \
	$(HOME)/.bashrc \
	$(HOME)/.bash_profile \
	$(XDG_HOME)/compton/compton.conf \
	$(XDG_HOME)/dunst/dunstrc \
	$(XDG_HOME)/fish/aliases.fish \
	$(XDG_HOME)/fish/colors.fish \
	$(XDG_HOME)/fish/config.fish \
	$(XDG_HOME)/fish/fish_prompt.fish \
	$(XDG_HOME)/fish/keybindings.fish \
	$(HOME)/.ghci \
	$(HOME)/.gnupg/gnupg.conf \
	$(HOME)/.haskeline \
	$(XDG_HOME)/git/config \
	$(XDG_HOME)/guix/channels.scm \
	$(XDG_HOME)/lynx/lynx.cfg \
	$(HOME)/.psqlrc \
	$(HOME)/.inputrc \
	$(XDG_HOME)/shepherd/init.scm \
	$(HOME)/.tmux.conf \
	$(HOME)/.tmux/tmuxline.conf \
	$(XDG_HOME)/xmobar/xmobar.hs \
	$(HOME)/.xsession \
	$(HOME)/.xmonad/xmonad.hs \
	$(XDG_HOME)/zathura/zathurarc

MODULES = \
	$(MODPROBE)/ath9k.conf \
	$(MODPROBE)/blacklist.conf \
	$(MODPROBE)/default.conf

softlink = ln -s $(1) $(2)

# ----------- top level commands -------------
.PHONY: install
install: | $(SOFTLINKS) ## Install $(SOFTLINKS) to $HOME (default)

.PHONY: modules
modules: $(MODPROBE) ## Install $(MODULES) to $(MODPROBE). Probably don't apply to you.

.PHONY: help
help:
	@grep -E '^[a-zA-Z_()$$. /-]+:.*?## .*$$' Makefile | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

# ------------ softlinks -------------
$(HOME)/.profile: | $(HOME) ## The profile for session initialization. Not posix compliant.
	$(call softlink,$(PWD)/.profile,$@)

$(HOME)/.emacs.d/init.el: | $(HOME)/.emacs.d ## Emacs initialization file
	$(call softlink,$(PWD)/emacs/init.el,$@)

$(XDG_HOME)/alacritty/alacritty.yml: | $(XDG_HOME)/alacritty ## Alacritty configuration
	$(call softlink,$(PWD)/alacritty/alacritty.yml,$@)

$(HOME)/.bashrc: | $(HOME) ## Bash configuration (per shell)
	$(call softlink,$(PWD)/bash/.bashrc,$@)

$(HOME)/.bash_profile: | $(HOME) ## Bash configuration (per session)
	$(call softlink,$(PWD)/bash/.bash_profile,$@)

$(XDG_HOME)/compton/compton.conf: | $(XDG_HOME)/compton ## Compton configuration
	$(call softlink,$(PWD)/compton/compton.conf,$@)

$(XDG_HOME)/dunst/dunstrc: | $(XDG_HOME)/dunst ## Dunst configuration
	$(call softlink,$(PWD)/dunst/dunstrc,$@)

$(XDG_HOME)/fish/aliases.fish: | $(XDG_HOME)/fish ## Fish aliases, abbreviations, and functions
	$(call softlink,$(PWD)/fish/aliases.fish,$@)

$(XDG_HOME)/fish/colors.fish: | $(XDG_HOME)/fish ## Fish color definitions
	$(call softlink,$(PWD)/fish/colors.fish,$@)

$(XDG_HOME)/fish/config.fish: | $(XDG_HOME)/fish ## Fish initialization
	$(call softlink,$(PWD)/fish/config.fish,$@)

$(XDG_HOME)/fish/fish_prompt.fish: | $(XDG_HOME)/fish ## Fish prompt
	$(call softlink,$(PWD)/fish/fish_prompt.fish,$@)

$(XDG_HOME)/fish/keybindings.fish: | $(XDG_HOME)/fish ## Fish keybindings
	$(call softlink,$(PWD)/fish/keybindings.fish,$@)

$(HOME)/.ghci: | $(HOME) ## ghci configuration
	$(call softlink,$(PWD)/ghci/.ghci,$@)

$(HOME)/.gnupg/gnupg.conf: | $(HOME)/.gnupg ## gnupg configuration
	$(call softlink,$(PWD)/gnupg/gnupg.conf,$@)

$(HOME)/.haskeline: | $(HOME) ## haskeline configuration (for ghci)
	$(call softlink,$(PWD)/ghci/.haskeline,$@)

$(XDG_HOME)/git/config: | $(XDG_HOME)/git ## Git configuration
	$(call softlink,$(PWD)/git/.gitconfig,$@)

$(XDG_HOME)/guix/channels.scm: | $(XDG_HOME)/guix ## Guix channel specification
	$(call softlink,$(PWD)/guix/channels.scm,$@)

$(XDG_HOME)/lynx/lynx.cfg: | $(XDG_HOME)/lynx ## Lynx configuration
	$(call softlink,$(PWD)/lynx/lynx.cfg,$@)

$(MODPROBE)/ath9k.conf: $(PWD)/modprobe.d/ath9k.conf | $(MODPROBE) ## Module for ath9k wireless card
	sudo cp $< $@
	sudo chown root $@

$(MODPROBE)/blacklist.conf: $(PWD)/modprobe.d/blacklist.conf | $(MODPROBE) ## Module to blacklist breaking hardware
	sudo cp $< $@
	sudo chown root $@

$(MODPROBE)/default.conf: $(PWD)/modprobe.d/default.conf | $(MODPROBE) ## Modules by for audio configuration and more
	sudo cp $< $@
	sudo chown root $@

$(HOME)/.psqlrc: | $(HOME) ## psql configuration
	$(call softlink,$(PWD)/psql/.psqlrc,$@)

$(HOME)/.inputrc: | $(HOME) ## readline configuration
	$(call softlink,$(PWD)/readline/.inputrc,$@)

$(XDG_HOME)/shepherd/init.scm: | $(XDG_HOME)/shepherd ## User shepherd configuration and services
	$(call softlink,$(PWD)/shepherd/init.scm,$@)

$(HOME)/.tmux.conf: | $(HOME) ## Tmux configuration
	$(call softlink,$(PWD)/tmux/.tmux.conf,$@)

$(HOME)/.tmux/tmuxline.conf: | $(HOME)/.tmux ## Tmux status line configuration
	$(call softlink,$(PWD)/tmux/tmuxline.conf,$@)

$(XDG_HOME)/xmobar/xmobar.hs: | $(XDG_HOME)/xmobar ## Xmobar configuration
	$(call softlink,$(PWD)/xmobar/xmobar.hs,$@)

$(HOME)/.xsession: | $(HOME) ## Loaded by gdm and other DMs on login
	$(call softlink,$(PWD)/xmonad/.xsession,$@)

$(HOME)/.xmonad/xmonad.hs: | $(HOME)/.xmonad ## XMonad configuration
	$(call softlink,$(PWD)/xmonad/xmonad.hs,$@)

$(XDG_HOME)/zathura/zathurarc: | $(XDG_HOME)/zathura ## Zathura configuration
	$(call softlink,$(PWD)/zathura/zathurarc,$@)

$(DIRS): ## Make sure containing directories exist
	mkdir -p $@

$(MODPROBE): ## Create a modprobe directory
	sudo mkdir -p $@
	sudo chown root $@
