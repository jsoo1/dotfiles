let
  elpa = pkgs: { elpaPackages, ... }:
    with elpaPackages; [
      consult
      csv-mode
      debbugs
      dired-git-info
      ediprolog
      sml-mode
      vertico
      which-key
      (xclip.overrideAttrs (o: {
        src = pkgs.stdenv.mkDerivation {
          name = o.src.name;
          src = o.src;
          patches = [ pkgs.emacs-xclip-soclip-support ];
          installPhase = ''
            mkdir xclip-${o.version}
            mv *.el xclip-${o.version}
            tar -cf $out xclip-${o.version}
          '';
        };
      }))
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
      fish-mode
      flycheck
      forge
      geiser
      go-mode
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
      vertico-prescient
      vimrc-mode
      web-mode
      wgrep
      xterm-color
      yaml-mode
    ];
  my-emacs-overlay = self: super:
    let
      emacs = self.mkGitEmacs "my-emacs-nox" ./emacs-rev.json {
        withNS = false;
        withX = false;
        withGTK2 = false;
        withGTK3 = false;
        nativeComp = true;
        withSQLite3 = true;
        withWebP = false;
      };
    in
    {
      my-emacs = (emacs.pkgs.emacsWithPackages (epkgs:
        builtins.concatMap (f: f epkgs) [ (elpa super) manual melpa ]));
    };
in
[ my-emacs-overlay ]
