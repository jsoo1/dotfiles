let
  elpa =
    pkgs:
    { elpaPackages, ... }:
    with elpaPackages;
    [
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
  manual =
    { manualPackages, ... }:
    with manualPackages;
    [
      # cedille - Broken
      agda2-mode
    ];
  melpa =
    pkgs:
    { melpaPackages, ... }:
    with melpaPackages;
    [
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
      dune
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
      groovy-mode
      guix
      haskell-mode
      haskell-snippets
      helpful
      highlight-indent-guides
      hydra
      idris-mode
      imenu-list
      jq-mode
      magit
      markdown-mode
      merlin
      meson-mode
      nodejs-repl
      ob-restclient
      orderless
      org-mime
      pcap-mode
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
  my-emacs-overlay =
    self: super:
    let
      emacs = self.mkGitEmacs "my-emacs-nox" ./emacs-rev.json {
        withNS = false;
        withX = false;
        withGTK3 = false;
        # Workaround for https://github.com/nix-community/emacs-overlay/issues/318
        withNativeCompilation = self.stdenv.hostPlatform.isDarwin;
        withSQLite3 = true;
        withWebP = false;
        withTreeSitter = true;
      };
    in
    {
      my-emacs = emacs.pkgs.emacsWithPackages (
        epkgs:
        builtins.concatMap (f: f epkgs) [
          (elpa super)
          manual
          (melpa super)
        ]
        ++ [
          epkgs.nix-ts-mode
          epkgs.treesit-grammars.with-all-grammars
        ]
      );
    };
in
[ my-emacs-overlay ]
