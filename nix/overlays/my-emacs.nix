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
  melpa = pkgs: { melpaPackages, ... }:
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
      (evil.overrideAttrs (o: {
        src = pkgs.fetchFromGitHub {
          owner = "jsoo1";
          repo = "evil";
          rev = "dc48c6917b05040815f6cafc3ae25f550f1cd11d";
          hash = "sha256-qap0zbqaf/uPt0IbyyG99G5kzcX0odta8jlr/y+1FRQ=";
        };
      }))
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
      (haskell-mode.overrideAttrs (o: {
        # Fixes use of removed function.
        # Check this next update
        src = pkgs.fetchFromGitHub {
          owner = "jsoo1";
          repo = "haskell-mode";
          rev = "3ece1f3c40b7428c976596e805a3384fa0673ee8";
          hash = "sha256-H1H26Kepxp1+cP9ysUUaBtHEnXwLL5icpfbOvNhnpNQ=";
        };
      }))
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
        withNativeCompilation = self.stdenv.hostPlatform.isLinux;
        withSQLite3 = true;
        withWebP = false;
        withTreeSitter = true;
      };
    in
    {
      my-emacs = (emacs.pkgs.emacsWithPackages (epkgs:
        builtins.concatMap (f: f epkgs) [
          (elpa super)
          manual
          (melpa super)
        ] ++ [
          epkgs.nix-ts-mode
        ] ++ (with super.tree-sitter-grammars; [
          tree-sitter-nix
          tree-sitter-haskell
        ])));
    };
in
[ my-emacs-overlay ]
