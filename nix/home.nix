{ pkgs, dotfiles, config, lib, ... }:
let
  isDarwin = pkgs.system == "x86_64-darwin";

  home = config.home.homeDirectory;
  username = config.home.username;

  env = import ./env.nix { inherit pkgs isDarwin; };

  feeds = {
    ".emacs.d/feeds".recursive = true;
    ".emacs.d/feeds".source = "${dotfiles}/rss";
  };

  ssh-auth-sock = "${home}/.ssh/auth_sock";

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

  gitconfig = {
    user = {
      name = "John Soo";
      email = "john.soo@arista.com";
      signingkey = "71F4C27CC2540312F69F553FD8A148F8CE4DDBC2";
    };
    core = { editor = "nvim"; };
    diff = { renames = true; };
    github = { user = "jsoo1"; };
    commit = { gpgsign = true; };
    branch = { autoSetupMerge = false; };

  };

  activation.emacs = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    $DRY_RUN_CMD ln -sf $VERBOSE_ARG $HOME/{dotfiles/nix,.emacs.d}/init.el
  '';

  linux-only = {
    inherit systemd;
    services.gpg-agent.enable = true;
    home.packages = env.shell-utilities ++ [
      pkgs.iosevka
      pkgs.home-manager
    ];
  };

  darwin-only.home = {
    inherit activation; file = feeds;
  };

in
lib.mkMerge [
  (lib.mkIf (!isDarwin) linux-only)
  (lib.mkIf isDarwin darwin-only)
  {
    home = {
      enableNixpkgsReleaseCheck = false;

      extraOutputsToInstall = [ "doc" "nc" ];

      packages = env.user;

      file = {
        ".ghci".source = "${dotfiles}/ghci/.ghci";
        ".haskeline".source = "${dotfiles}/ghci/.haskeline";
        ".psqlrc".source = "${dotfiles}/psql/.psqlrc";
        ".vimrc".source = "${dotfiles}/minimal/.vimrc";
      };
    };

    xdg.configFile = {
      "tmux/tmux.conf".source = "${dotfiles}/nix/.tmux.conf";
      "procps/toprc".source = "${dotfiles}/top/toprc";
    };

    programs = {
      autojump.enable = true;
      bash = import ./bash.nix { inherit config lib ssh-auth-sock isDarwin; };
      bat.enable = true;
      direnv.enable = true;
      direnv.enableBashIntegration = true;
      emacs.enable = true;
      emacs.package = pkgs.my-emacs;
      git.enable = true;
      git.extraConfig = gitconfig;
      gpg.enable = true;
      htop.enable = true;
      jq.enable = true;
      skim.defaultOptions = [ "-m" "--color=bw" "--layout=reverse" ];
      skim.enable = true;
      tmux.enable = true;
      tmux.package = pkgs.tmux;
    };
  }
]
