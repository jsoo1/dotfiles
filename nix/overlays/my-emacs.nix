let
  elpa = { elpaPackages, ... }:
    with elpaPackages; [
      consult
      csv-mode
      debbugs
      dired-git-info
      ediprolog
      eglot
      let-alist
      project
      sml-mode
      vertico
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
      cql-mode
      dhall-mode
      diredfl
      docker
      dockerfile-mode
      editorconfig
      elf-mode
      elfeed
      elpher
      embark
      embark-consult
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
      idris-mode
      imenu-list
      magit
      markdown-mode
      merlin
      multi-term
      nix-mode
      nodejs-repl
      ob-restclient
      orderless
      org-mime
      origami
      popper
      proof-general
      protobuf-mode
      psc-ide
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
  my-emacs-overlay = self: _:
    let
      emacs = (self.emacsGit.override {
        withNS = false;
        withX = false;
        withGTK2 = false;
        withGTK3 = false;
        nativeComp = true;
        withSQLite3 = true;
        withWebP = false;
      }).overrideAttrs (o: {
        src = o.src // {
          rev = "49b61405582edaa1cda05ea37b056d46b423271b";
          sha256 = "sha256-dJUlaNQhKGYhd474JQ5pk8naqLLI958U8Y3CPDffF2U=";
        };
      });
    in
    {
      my-emacs = emacs.pkgs.emacsWithPackages (epkgs:
        builtins.concatMap (f: f epkgs) [ elpa manual melpa ]);
    };
in
[ my-emacs-overlay ]
