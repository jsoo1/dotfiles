let
  elpa = { elpaPackages, ... }:
    with elpaPackages; [
      csv-mode
      debbugs
      dired-git-info
      ediprolog
      let-alist
      project
      seq
      sml-mode
      xclip
      xref
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
      editorconfig
      elfeed
      emms
      evil-replace-with-register
      exec-path-from-shell
      graphql-mode
      highlight-indent-guides
      hydra
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
      purescript-mode
      racket-mode
      redis
      restclient
      shackle
      systemd
      terraform-mode
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
      company
      company-coq
      company-math
      diredfl
      docker
      dockerfile-mode
      eglot
      elf-mode
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
    # 2021-11-22
    rev = "c7f51932424258572ab6790c3250384c8524a4c8";
  }));
  my-emacs-overlay = (self: super: {
    my-emacs = super.emacs-nox.pkgs.emacsWithPackages (epkgs:
      (builtins.concatMap (f: f epkgs) [ elpa manual melpa melpaStable ]));
  });
in [ emacs-overlay my-emacs-overlay ]
