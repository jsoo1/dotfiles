let
  elpa = { elpaPackages, ... }:
    with elpaPackages; [
      csv-mode
      debbugs
      ediprolog
      let-alist
      project
      seq
      sml-mode
      undo-tree
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
      dired-git
      editorconfig
      emms
      evil
      evil-anzu
      evil-collection
      evil-commentary
      evil-escape
      evil-leader
      evil-org
      evil-replace-with-register
      evil-surround
      exec-path-from-shell
      graphql-mode
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
      elfeed
      elpher
      emmet-mode
      envrc
      eredis
      eshell-syntax-highlighting
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
    # 2021-10-18
    rev = "c60e01cf4e8d5cb2cc48c6a0ac1138dd57ad7bc7";
  }));
  my-emacs-overlay = (self: super: {
    my-emacs = (super.emacsGit-nox.overrideAttrs (_: {
      src = super.fetchFromSavannah {
        repo = "emacs";
        # emacs-28 branch on 2021-10-19
        rev = "d742cc3c204ba0adeb9600d236a0e454e35a42ff";
        sha256 = "0hs9qf8fldpqzyw3xcyc1sxaigp6w1f81xh0p5bi5qnvzj8flchn";
      };
    })).pkgs.emacsWithPackages (epkgs:
      (builtins.concatMap (f: f epkgs) [ elpa manual melpa melpaStable ]));
  });
in [ emacs-overlay my-emacs-overlay ]
