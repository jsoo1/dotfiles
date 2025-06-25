{
  pkgs,
  soclip,
  config,
  lib,
  ...
}:
let
  inherit (pkgs.stdenv.hostPlatform) isLinux isDarwin;

  gitconfig = {
    user = {
      name = "John Soo";
      email = "johh.soo@arista.com";
      signingkey = "05BB AC5F A7FE 5527 3FE2  AC2A A3D4 605B 5EF5 F9C7";
    };
    core.editor = "${pkgs.neovim}/bin/nvim";
    diff.renames = true;
    fetch.prune = true;
    github.user = "jsoo1";
    commit.gpgsign = true;
    commit.cleanup = "scissors";
    branch.autoSetupMerge = false;
    advice.detachedHead = false;
    init.defaultBranch = "release";
    url."git@github.com:".insteadOf = "https://github.com/";
    rebase.updateRefs = true;
  };

  jjconfig = gitconfig // {
    ui.pager = "bat --paging=auto";
    signing = {
      sign-all = true;
      backend = "gpg";
      key = "B20E410388BD3540";
    };
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
    extraOutputsToInstall = [ "doc" ];

    stateVersion = "22.05";
    enableNixpkgsReleaseCheck = false;

    packages =
      lib.concatLists [
        config.haskell-utilities
        config.c-utilities
        config.nix-utilities
        config.socket-utilities
        config.terraform-utilities
        config.experimental-utilities
      ]
      ++ lib.optionals isLinux (
        lib.concatLists [
          config.shell-utilities
          [
            pkgs.iosevka
            pkgs.procps
          ]
        ]
      )
      ++ lib.optionals isDarwin (
        lib.concatLists [
          config.macos-quirks
          config.remarkable-utilities
        ]
      );

    file = {
      ".ghci".source = "${../ghci/.ghci}";
      ".haskeline".source = "${../ghci/.haskeline}";
      ".psqlrc".source = "${../psql/.psqlrc}";
      ".vimrc".source = "${../minimal/.vimrc}";
      ".emacs.d/eshell/alias".source = "${../emacs/eshell/alias}";
      ".emacs.d/feeds" = lib.mkIf isDarwin {
        recursive = true;
        source = "${../rss}";
      };
    };

    activation.emacs = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      $DRY_RUN_CMD ln -sfv $VERBOSE_ARG ${config.home.homeDirectory}/{dotfiles/nix,.emacs.d}/init.el
    '';
  };

  xdg.enable = true;

  xdg.configFile = {
    "nvim/init.vim".source = "${../minimal/.vimrc}";
    "tmux/tmux.conf".source = pkgs.runCommand "tmux.conf" { } ''
      cat <<EOF > $out
      $(cat "${../nix/.tmux.conf}")

      # clipboard for remotes
      set -s copy-command '${if isDarwin then "pbcopy" else "socopy"}'
      EOF
    '';
    "procps/toprc".source = "${../top/toprc}";
  };

  programs = {
    autojump.enable = true;
    bat.enable = true;
    bat.config.theme = "Solarized (dark)";
    bat.config.style = "plain";
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
    jujutsu.enable = true;
    jujutsu.settings = jjconfig;
    soclip.enable = true;
    skim.defaultOptions = [
      "-m"
      "--color=bw"
      "--layout=reverse"
    ];
    skim.enable = true;
    tmux.enable = true;
    tmux.package = pkgs.tmux;
  };

  services.soclip.enable = isDarwin;

  services.gpg-agent.enable = isLinux;

  systemd.user.sockets.emacs = {
    Unit.Description = "Emacs socket";
    Socket = {
      DirectoryMode = "0700";
      ListenStream = "%t/emacs/${config.home.username}";
      SocketMode = "0600";
    };
    Install.WantedBy = [ "sockets.target" ];
  };

  systemd.user.services.emacs = {
    Unit = {
      Description = "Emacs Daemon";
      Documentation = "man:emacs(1)";
      Requires = [ "emacs.socket" ];
    };
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
        NIX_PATH = "nixpkgs=${pkgs.path}:ssh-auth-sock=${config.ssh-auth-sock}";
      };
      KeepAlive = true;
      ProgramArguments = [
        "/bin/sh"
        "-c"
        "/bin/wait4path ${pkgs.my-emacs}/bin/emacs &amp;&amp; exec ${pkgs.my-emacs}/bin/emacs --fg-daemon=${config.home.username} --debug-init"
      ];
      SoftResourceLimits.NumberOfFiles = 128000;
      HardResourceLimits.NumberOfFiles = 524288;
    };
  };
}
