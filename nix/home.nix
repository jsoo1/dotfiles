{ pkgs ? import ./pin.nix { }, config, lib, ... }:
let
  isDarwin = builtins.currentSystem == "x86_64-darwin";

  home = config.home.homeDirectory;
  username = config.home.username;
  dotfiles = "${home}/dotfiles";

  env = import ./env.nix { inherit pkgs isDarwin; };

  feeds = {
    ".emacs.d/feeds".recursive = true;
    ".emacs.d/feeds".source = "${dotfiles}/rss";
  };

  ssh-auth-sock = "${home}/.ssh/auth_sock";

  services.gpg-agent = {
    enable = true;
    verbose = true;
  };

  launchd-agents = {
    "Library/LaunchAgents/org.gnu.emacs.plist".source =
      "${dotfiles}/nix/org.gnu.emacs.plist";
  };

  systemd.user.services.emacs = {
    Unit.Description = "Emacs Daemon";
    Unit.Documentation = "man:emacs(1)";
    Install.WantedBy = [ "default.target" ];
    Service = {
      Environment = ''SSH_AUTH_SOCK="${ssh-auth-sock}"'';
      ExecStart = "${pkgs.my-emacs}/bin/emacs --fg-daemon=${username}";
      ExecStop = "${pkgs.coreutils}/bin/kill -9 $MAINPID";
    };
  };

  activation.emacs = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    $DRY_RUN_CMD ln -sf $VERBOSE_ARG $HOME/{dotfiles/nix,.emacs.d}/init.el
  '';

in lib.optionalAttrs (!isDarwin) { inherit systemd services; } // {
  home = lib.optionalAttrs isDarwin {
    inherit activation;
    enableNixpkgsReleaseCheck = false;
  } // {
    extraOutputsToInstall = [ "doc" "nc" ];

    packages = env.user ++ lib.optionals (!isDarwin) env.shell-utilities;

    file = lib.optionalAttrs isDarwin (feeds // launchd-agents) // {
      ".ghci".source = "${dotfiles}/ghci/.ghci";
      ".haskeline".source = "${dotfiles}/ghci/.haskeline";
      ".psqlrc".source = "${dotfiles}/psql/.psqlrc";
      ".vimrc".source = "${dotfiles}/minimal/.vimrc";
      ".tmux.conf".source = "${dotfiles}/nix/.tmux.conf";
    };
  };

  programs = lib.optionalAttrs isDarwin { autojump.enable = true; } // {
    bat.enable = true;
    direnv.enable = true;
    direnv.enableBashIntegration = true;
    emacs.enable = true;
    emacs.package = pkgs.my-emacs;
    gpg.enable = true;
    htop.enable = true;
    jq.enable = true;
    skim.defaultOptions = [ "-m" "--color=bw" ];
    skim.enable = true;
    bash = import ./bash.nix { inherit config lib ssh-auth-sock isDarwin; };
  };
}
