let
  # 2021-08-15
  nix-rev = "3ab8ce12c2db31268f579c11727d9c63cfee2eee";
  mozilla-overlay = import (builtins.fetchTarball
    "https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz");
  nix-src = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${nix-rev}.tar.gz";
    sha256 = "13fx0yb95gxyk0jyvgrxv2yp4fj54g7nzrlvjfc8slx9ccqd2v86";
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
      counsel-projectile
      cql-mode
      dhall-mode
      dired-git
      evil-replace-with-register
      exec-path-from-shell
      graphql-mode
      ibuffer-projectile
      merlin
      multi-term
      ob-restclient
      origami
      projectile
      proof-general
      psc-ide
      racket-mode
      redis
      restclient
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
in import nix-src {
  overlays = [
    mozilla-overlay
    (import (builtins.fetchGit {
      url = "https://github.com/nix-community/emacs-overlay";
      ref = "master";
      rev = "2b083adda6867e7c3812c84a10c04d1476c1ac81";
    }))
    (self: super: {
      restream = super.restream.overrideAttrs (_: {
        installPhase = ''
          runHook preInstall

          install -D restream.arm.static $out/libexec/restream.arm.static
          install -D reStream.sh $out/bin/restream

          runHook postInstall
        '';
        patches = [ ./nix/restream-invert.patch ];
      });
    })
    (self: super: {
      my-emacs = super.emacs.pkgs.emacsWithPackages (epkgs:
        (builtins.concatMap (f: f epkgs) [ elpa manual melpa melpaStable ]));
    })
  ];
}
