{ pkgs, dotfiles, soclip, config, lib, ... }:
let
  inherit (pkgs.stdenv.hostPlatform) isLinux isDarwin;

  gitconfig = {
    user = {
      name = "John Soo";
      email = "john.soo@arista.com";
      signingkey = "B2048DDD7FD2A52E";
    };
    core.editor = "${pkgs.neovim}/bin/nvim";
    diff.renames = true;
    github.user = "jsoo1";
    commit.gpgsign = true;
    branch.autoSetupMerge = false;
    advice.detachedHead = false;
    init.defaultBranch = "release";
  };

in
{
  imports = [
    ./env.nix
    ./bash.nix
    soclip.homeManagerModules.default
    ./ssh-auth-sock.nix
  ];

  home = {
    extraOutputsToInstall = [ "doc" "nc" ];

    stateVersion = "22.05";

    packages = lib.concatLists [
      config.haskell-utilities
      config.c-utilities
      config.nix-utilities
      config.socket-utilities
      config.terraform-utilities
    ] ++ lib.optionals isLinux (lib.concatLists [
      config.shell-utilities
      [ pkgs.iosevka pkgs.procps ]
    ]) ++ lib.optionals isDarwin (lib.concatLists [
      config.macos-quirks
      config.remarkable-utilities
    ]);

    file = {
      ".ghci".source = "${dotfiles}/ghci/.ghci";
      ".haskeline".source = "${dotfiles}/ghci/.haskeline";
      ".psqlrc".source = "${dotfiles}/psql/.psqlrc";
      ".vimrc".source = "${dotfiles}/minimal/.vimrc";
      ".emacs.d/eshell/alias".source = "${dotfiles}/emacs/eshell/alias";
      ".emacs.d/feeds" = lib.mkIf isDarwin {
        recursive = true;
        source = "${dotfiles}/rss";
      };
    };

    activation.emacs = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      $DRY_RUN_CMD ln -sfv $VERBOSE_ARG ${config.home.homeDirectory}/{dotfiles/nix,.emacs.d}/init.el
    '';
  };

  xdg.enable = true;

  xdg.configFile = {
    "nvim/init.vim".source = "${dotfiles}/minimal/.vimrc";
    "tmux/tmux.conf".source = pkgs.runCommand "tmux.conf" { } ''
      cat <<EOF > $out
      $(cat "${dotfiles}/nix/.tmux.conf")

      # clipboard for remotes
      set -s copy-command '${if isDarwin then "pbcopy" else "socopy"}'
      EOF
    '';
    "procps/toprc".source = "${dotfiles}/top/toprc";
  };

  programs = {
    autojump.enable = true;
    bat.enable = true;
    bat.config.theme = "Solarized (dark)";
    direnv.enable = true;
    direnv.enableBashIntegration = true;
    emacs.enable = true;
    emacs.package = pkgs.my-emacs;
    git.enable = true;
    git.package = pkgs.git;
    git.extraConfig = gitconfig;
    gpg.enable = true;
    htop.enable = isDarwin;
    jq.enable = true;
    soclip.enable = true;
    skim.defaultOptions = [ "-m" "--color=bw" "--layout=reverse" ];
    skim.enable = true;
    tmux.enable = true;
    tmux.package = pkgs.tmux;
  };

  services.soclip.enable = isDarwin;

  services.gpg-agent.enable = isLinux;

  systemd.user.services.emacs = {
    Unit.Description = "Emacs Daemon";
    Unit.Documentation = "man:emacs(1)";
    Install.WantedBy = [ "default.target" ];
    Service = {
      Environment = ''SSH_AUTH_SOCK="${config.ssh-auth-sock}"'';
      ExecStart = "${pkgs.my-emacs}/bin/emacs --fg-daemon=${config.home.username}";
      ExecStop = "${pkgs.coreutils}/bin/kill -9 $MAINPID";
    };
  };

  launchd.agents.emacs = {
    enable = true;

    config = {
      EnvironmentVariables = {
        SSH_AUTH_SOCK = config.ssh-auth-sock;
        NIX_PATH = "nixpkgs=${pkgs.path}";
      };
      KeepAlive = true;
      ProgramArguments = [
        "/bin/sh"
        "-c"
        "/bin/wait4path ${pkgs.my-emacs}/bin/emacs &amp;&amp; exec ${pkgs.my-emacs}/bin/emacs --fg-daemon=${config.home.username}"
      ];
    };
  };
}
