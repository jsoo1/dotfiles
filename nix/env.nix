{ pkgs ? import ./pin.nix { }
, isDarwin ? builtins.currentSystem == "x86_64-darwin"
}:
let
  inherit (pkgs)
    bashCompletion bashInteractive bottom dogdns exa fd gawk gdb ghcid git
    haskell-language-server iosevka less libressl neovim nix-diff
    nix-prefetch nixpkgs-fmt peep rage restream ripgrep rnix-lsp rr shellcheck socat
    tealdeer terraform-lsp watch;
  haskell-utilities = [ ghcid haskell-language-server ];
  c-utilities = [ gdb ] ++ pkgs.lib.optional (!isDarwin) rr;
  macos-quirks = [ bashInteractive bashCompletion less neovim ripgrep fd git rage dogdns ];
  nix-utilities = [ nixpkgs-fmt nix-diff nix-prefetch rnix-lsp ];
  remarkable-utilities = [ restream ];
  shell-utilities = [
    bashCompletion
    bottom
    dogdns
    exa
    fd
    gawk
    git
    neovim
    peep
    rage
    ripgrep
    shellcheck
    tealdeer
    watch
  ];
  socket-utilities = [
    libressl # see "nc" in extraOutputsToInstall
    socat
  ];
  terraform-utilities = [ terraform-lsp ];
in
{
  inherit haskell-utilities c-utilities macos-quirks nix-utilities
    remarkable-utilities shell-utilities socket-utilities terraform-utilities;

  user = builtins.concatLists [
    haskell-utilities
    c-utilities
    nix-utilities
    socket-utilities
    terraform-utilities
    (pkgs.lib.optionals isDarwin
      (macos-quirks ++ remarkable-utilities))
  ];
}
