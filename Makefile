XDG_HOME ?= $(HOME)/.config
RUN ?= /run

DIRS = \
	$(HOME) \
	$(HOME)/.emacs.d \
	$(XDG_HOME)/alacritty \
	$(XDG_HOME)/compton \
	$(XDG_HOME)/fish \
	$(XDG_HOME)/git \
	$(XDG_HOME)/guix \
	$(XDG_HOME)/lynx \
	$(XDG_HOME)/shepherd \
	$(HOME)/.tmux \
	$(XDG_HOME)/xmobar \
	$(HOME)/.xmonad \
	$(XDG_HOME)/zathura

MODPROBE = \
	$(RUN)/modprobe.d

SOFTLINKS = \
	$(HOME)/.profile \
	$(HOME)/.emacs.d/init.el \
	$(XDG_HOME)/alacritty/alacritty.yml \
	$(HOME)/.bashrc \
	$(HOME)/.bash_profile \
	$(XDG_HOME)/fish/aliases.fish \
	$(XDG_HOME)/fish/colors.fish \
	$(XDG_HOME)/fish/config.fish \
	$(XDG_HOME)/fish/fish_prompt.fish \
	$(XDG_HOME)/fish/keybindings.fish \
	$(HOME)/.ghci \
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
	$(RUN)/modprobe.d/ath9k.conf \
	$(RUN)/modprobe.d/blacklist.conf \
	$(RUN)/modprobe.d/default.conf

softlink = ln -s $(1) $(2)

# ----------- top level commands -------------
.PHONY: install
install: | $(SOFTLINKS) ## Install the dotfiles to $HOME (default)

.PHONY: modprobe
modprobe: $(MODPROBE) ## Some hardware specific modules. Probably don't apply to you.

.PHONY: help
help:
	@grep -E '^[a-zA-Z_ -]+:.*?## .*$$' Makefile | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

# ------------ softlinks -------------
$(HOME)/.profile: | $(HOME)
	$(call softlink,$(PWD)/.profile,$@)

$(HOME)/.emacs.d/init.el: | $(HOME)/.emacs.d
	$(call softlink,$(PWD)/emacs/init.el,$@)

$(XDG_HOME)/alacritty/alacritty.yml: | $(XDG_HOME)/alacritty
	$(call softlink,$(PWD)/alacritty/alacritty.yml,$@)

$(HOME)/.bashrc: | $(HOME)
	$(call softlink,$(PWD)/bash/.bashrc,$@)

$(HOME)/.bash_profile: | $(HOME)
	$(call softlink,$(PWD)/bash/.bash_profile,$@)

$(XDG_HOME)/compton/compton.conf: | $(XDG_HOME)/compton
	$(call softlink,$(PWD)/compton/compton.conf,$@)

$(XDG_HOME)/fish/aliases.fish: | $(XDG_HOME)/fish
	$(call softlink,$(PWD)/fish/aliases.fish,$@)

$(XDG_HOME)/fish/colors.fish: | $(XDG_HOME)/fish
	$(call softlink,$(PWD)/fish/colors.fish,$@)

$(XDG_HOME)/fish/config.fish: | $(XDG_HOME)/fish
	$(call softlink,$(PWD)/fish/config.fish,$@)

$(XDG_HOME)/fish/fish_prompt.fish: | $(XDG_HOME)/fish
	$(call softlink,$(PWD)/fish/fish_prompt.fish,$@)

$(XDG_HOME)/fish/keybindings.fish: | $(XDG_HOME)/fish
	$(call softlink,$(PWD)/fish/keybindings.fish,$@)

$(HOME)/.ghci: | $(HOME)
	$(call softlink,$(PWD)/ghci/.ghci,$@)

$(HOME)/.haskeline: | $(HOME)
	$(call softlink,$(PWD)/ghci/.haskeline,$@)

$(XDG_HOME)/git/config: | $(XDG_HOME)/git
	$(call softlink,$(PWD)/git/.gitconfig,$@)

$(XDG_HOME)/guix/channels.scm: | $(XDG_HOME)/guix
	$(call softlink,$(PWD)/guix/channels.scm,$@)

$(XDG_HOME)/lynx/lynx.cfg: | $(XDG_HOME)/lynx
	$(call softlink,$(PWD)/lynx/lynx.cfg,$@)

$(RUN)/modprobe.d/ath9k.conf: $(PWD)/modprobe.d/ath9k.conf | $(RUN)/modprobe.d
	sudo cp $< $@
	sudo chown root $@

$(RUN)/modprobe.d/blacklist.conf: $(PWD)/modprobe.d/blacklist.conf | $(RUN)/modprobe.d
	sudo cp $< $@
	sudo chown root $@

$(RUN)/modprobe.d/default.conf: $(PWD)/modprobe.d/default.conf | $(RUN)/modprobe.d
	sudo cp $< $@
	sudo chown root $@

$(HOME)/.psqlrc: | $(HOME)
	$(call softlink,$(PWD)/psql/.psqlrc,$@)

$(HOME)/.inputrc: | $(HOME)
	$(call softlink,$(PWD)/readline/.inputrc,$@)

$(XDG_HOME)/shepherd/init.scm: | $(XDG_HOME)/shepherd
	$(call softlink,$(PWD)/shepherd/init.scm,$@)

$(HOME)/.tmux.conf: | $(HOME)
	$(call softlink,$(PWD)/tmux/.tmux.conf,$@)

$(HOME)/.tmux/tmuxline.conf: | $(HOME)/.tmux
	$(call softlink,$(PWD)/tmux/tmuxline.conf,$@)

$(XDG_HOME)/xmobar/xmobar.hs: | $(XDG_HOME)/xmobar
	$(call softlink,$(PWD)/xmobar/xmobar.hs,$@)

$(HOME)/.xsession: | $(HOME)
	$(call softlink,$(PWD)/xmonad/.xsession,$@)

$(HOME)/.xmonad/xmonad.hs: | $(HOME)/.xmonad
	$(call softlink,$(PWD)/xmonad/xmonad.hs,$@)

$(XDG_HOME)/zathura/zathurarc: | $(XDG_HOME)/zathura
	$(call softlink,$(PWD)/zathura/zathurarc,$@)

$(DIRS):
	mkdir -p $@

$(MODPROBE):
	sudo mkdir -p $@
	sudo chown root $@
