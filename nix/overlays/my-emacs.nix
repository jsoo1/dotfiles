let
  elpa = { elpaPackages, ... }:
    with elpaPackages; [
      csv-mode
      debbugs
      dired-git-info
      ediprolog
      eglot
      let-alist
      project
      sml-mode
      which-key
      xclip
      xref
      yasnippet
    ];
  manual = { manualPackages, ... }:
    with manualPackages;
    [
      # cedille - Broken
      agda2-mode
    ];
  melpa = { melpaPackages, ... }:
    with melpaPackages; [
      aio
      anzu
      base16-theme
      clojure-mode
      cmake-mode
      company
      company-coq
      company-math
      counsel-projectile
      cql-mode
      dhall-mode
      diredfl
      docker
      dockerfile-mode
      editorconfig
      elf-mode
      elfeed
      elpher
      emmet-mode
      emms
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
      evil-replace-with-register
      evil-surround
      exec-path-from-shell
      f
      fill-column-indicator
      fish-completion
      fish-mode
      flycheck
      forge
      geiser
      go-mode
      goto-chg
      graphql-mode
      graphviz-dot-mode
      guix
      haskell-mode
      haskell-snippets
      helpful
      highlight-indent-guides
      hydra
      ibuffer-projectile
      idris-mode
      imenu-list
      ivy
      ivy-prescient
      libgit
      magit
      magit-libgit
      markdown-mode
      merlin
      multi-term
      nix-mode
      nodejs-repl
      ob-restclient
      org-mime
      origami
      popper
      projectile
      proof-general
      protobuf-mode
      psc-ide
      pulseaudio-control
      purescript-mode
      racket-mode
      redis
      reformatter
      restclient
      rust-mode
      sbt-mode
      scala-mode
      shackle
      slime
      slime-company
      systemd
      terraform-mode
      tmux-pane
      tuareg
      uuidgen
      vimrc-mode
      web-mode
      wgrep
      xterm-color
      yaml-mode
    ];
  my-emacs-overlay = self: super:
    let
      emacs = self.mkGitEmacs "my-emacs-git-nox" ./emacs-rev.json {
        withNS = false;
        withX = false;
        withGTK2 = false;
        withGTK3 = false;
        nativeComp = true;
        withSQLite3 = true;
      };
    in
    {
      my-emacs = emacs.pkgs.emacsWithPackages (epkgs:
        builtins.concatMap (f: f epkgs) [ elpa manual melpa ]);
    };
in
[ my-emacs-overlay ]
