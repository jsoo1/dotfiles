{ pkgs ? import ../pin.nix }:
pkgs.mkShell {
  name = "emacs-config-shell";
  buildInputs = [ emacs pkgs.glibcLocales pkgs.postgresql ];
  shellHook = ''
    unset EMACSLOADPATH
    alias emacs="emacs -q -l $HOME/dotfiles/emacs/init-nix.el"
  '';
}
