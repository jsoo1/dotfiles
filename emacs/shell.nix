let
  # 2021-08-15
  nix-rev = "3ab8ce12c2db31268f579c11727d9c63cfee2eee";
  nix-src = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${nix-rev}.tar.gz";
    sha256 = "13fx0yb95gxyk0jyvgrxv2yp4fj54g7nzrlvjfc8slx9ccqd2v86";
  };
  pkgs = import nix-src {
    overlays = [
      (import (builtins.fetchGit {
        url = "https://github.com/nix-community/emacs-overlay";
        ref = "master";
        rev = "2b083adda6867e7c3812c84a10c04d1476c1ac81";
      }))
    ];
  };
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
      cql-mode
      dhall-mode
      dired-git
      # evil-tmux-navigator - Missing
      evil-replace-with-register
      graphql-mode
      merlin
      multi-term
      ob-restclient
      origami
      proof-general
      psc-ide
      racket-mode
      redis
      restclient
      systemd
      vimrc-mode
      uuidgen
    ];
  melpaStable = { melpaStablePackages, ... }:
    with melpaStablePackages; [
      aio
      anzu
      clojure-mode
      cmake-mode
      company
      company-coq
      company-math
      counsel-projectile
      dash
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
      ibuffer-projectile
      idris-mode
      imenu-list
      ivy
      magit
      markdown-mode
      nix-mode
      nodejs-repl
      org-mime
      ivy-prescient
      projectile
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
  emacs = pkgs.emacs.pkgs.emacsWithPackages (epkgs:
    (builtins.concatMap (f: f epkgs) [ elpa manual melpa melpaStable ]));
in pkgs.mkShell {
  name = "emacs-config-shell";
  buildInputs = [ emacs pkgs.glibcLocales ];
  shellHook = ''
    unset EMACSLOADPATH
    alias emacs="emacs -q -l $HOME/dotfiles/emacs/init-nix.el"
  '';
}
