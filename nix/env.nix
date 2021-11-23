{ pkgs ? import ./pin.nix { }
, isDarwin ? builtins.currentSystem == "x86_64-darwin" }:
let
  inherit (pkgs)
    bashCompletion bashInteractive dogdns exa fd gawk gdb ghcid git
    haskell-language-server iosevka libressl neovim nix-diff nix-prefetch nixfmt
    rage restream ripgrep rnix-lsp rr shellcheck socat terraform-lsp tmux watch;
  fonts = [ iosevka ];
  haskell-utilities = [ ghcid haskell-language-server ];
  c-utilities = [ gdb ] ++ pkgs.lib.optional (!isDarwin) rr;
  macos-quirks = [ bashInteractive ];
  nix-utilities = [ nixfmt nix-diff nix-prefetch rnix-lsp ];
  remarkable-utilities = [ restream ];
  shell-utilities = [
    bashCompletion
    dogdns
    exa
    fd
    gawk
    git
    neovim
    rage
    ripgrep
    shellcheck
    tmux
    watch
  ];
  socket-utilities = [
    libressl # see "nc" in extraOutputsToInstall
    socat
  ];
  terraform-utilities = [ terraform-lsp ];
in {
  inherit fonts haskell-utilities c-utilities macos-quirks nix-utilities
    remarkable-utilities shell-utilities socket-utilities terraform-utilities;

  user = builtins.concatLists [
    haskell-utilities
    c-utilities
    nix-utilities
    socket-utilities
    terraform-utilities
    (pkgs.lib.optionals isDarwin
      (fonts ++ macos-quirks ++ remarkable-utilities))
  ];
}
