let
  elpa = { elpaPackages, ... }:
    with elpaPackages; [
      csv-mode
      debbugs
      ediprolog
      let-alist
      seq
      sml-mode
      xclip
    ];
  manual = { manualPackages, ... }:
    with manualPackages;
    [ # cedille - Broken
      agda2-mode
    ];
  melpa = { melpaPackages, ... }:
    with melpaPackages; [
      counsel-projectile
      cql-mode
      dhall-mode
      dired-git
      editorconfig
      emms
      evil-replace-with-register
      exec-path-from-shell
      graphql-mode
      ibuffer-projectile
      merlin
      multi-term
      ob-restclient
      origami
      popper
      projectile
      proof-general
      psc-ide
      pulseaudio-control
      racket-mode
      redis
      restclient
      shackle
      systemd
      tmux-pane
      vimrc-mode
      uuidgen
    ];
  melpaStable = { melpaStablePackages, ... }:
    with melpaStablePackages; [
      aio
      anzu
      base16-theme
      clojure-mode
      cmake-mode
      company
      company-coq
      company-math
      diredfl
      docker
      dockerfile-mode
      eglot
      elf-mode
      elfeed
      elpher
      emmet-mode
      envrc
      eredis
      eshell-syntax-highlighting
      evil
      evil-anzu
      evil-collection
      evil-commentary
      evil-escape
      evil-leader
      evil-org
      evil-surround
      f
      fill-column-indicator
      fish-completion
      fish-mode
      flycheck
      forge
      geiser
      goto-chg
      graphviz-dot-mode
      guix
      haskell-mode
      haskell-snippets
      helpful
      idris-mode
      imenu-list
      ivy
      magit
      markdown-mode
      nix-mode
      nodejs-repl
      org-mime
      ivy-prescient
      reformatter
      rust-mode
      s
      slime
      slime-company
      solarized-theme
      terraform-mode
      tuareg
      web-mode
      wgrep
      which-key
      xterm-color
      yaml-mode
      yasnippet
    ];
  emacs-overlay = (import (builtins.fetchGit {
    url = "https://github.com/nix-community/emacs-overlay";
    ref = "master";
    rev = "2b083adda6867e7c3812c84a10c04d1476c1ac81";
  }));
  my-emacs-overlay = (self: super: {
    my-emacs = super.emacs-nox.pkgs.emacsWithPackages (epkgs:
      (builtins.concatMap (f: f epkgs) [ elpa manual melpa melpaStable ]));
  });
in [ emacs-overlay my-emacs-overlay ]
