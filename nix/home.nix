{ pkgs, dotfiles, config, lib, ... }:
let
  inherit (pkgs.stdenv.hostPlatform) isLinux isDarwin;

  gitconfig = {
    user = {
      name = "John Soo";
      email = "john.soo@arista.com";
      signingkey = "71F4C27CC2540312F69F553FD8A148F8CE4DDBC2";
    };
    core = { editor = "${pkgs.neovim}/bin/nvim"; };
    diff = { renames = true; };
    github = { user = "jsoo1"; };
    commit = { gpgsign = true; };
    branch = { autoSetupMerge = false; };
  };

in
{
  imports = [
    ./env.nix
    ./bash.nix
    ./netclip.nix
    ./ssh-auth-sock.nix
  ];

  home = {
    extraOutputsToInstall = [ "doc" "nc" ];

    stateVersion = "22.05";

    packages = lib.concatLists [
      config.haskell-utilities
      config.c-utilities
      config.go-utilities
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
      set -s copy-command '${if isDarwin then "pbcopy" else "netclip"}'
      EOF
    '';
    "procps/toprc ".source = "${dotfiles}/top/toprc";
    "oil/oshrc".text = ''
      ${lib.optionalString isDarwin ''
         # Avoids errors in /etc/bashrc_Apple_Terminal (unused anyways)
         TERM_PROGRAM_OLD="$TERM_PROGRAM"
         TERM_PROGRAM=junk
         source /etc/bashrc
         TERM_PROGRAM="$TERM_PROGRAM_OLD"
      ''}

      ${lib.concatStringsSep "\n" (lib.mapAttrsToList (var: val: ''export ${var}="${val}"'')
        config.programs.bash.sessionVariables)}

      ${lib.concatStringsSep "\n" (lib.mapAttrsToList (var: val: "alias ${var}=${lib.escapeShellArg val}")
        config.programs.bash.shellAliases)}

      export SKIM_DEFAULT_OPTIONS="${lib.concatStringsSep " " config.programs.skim.defaultOptions}"

      alias tmn='eval "$(tmux-projects)"'

      ${config.programs.bash.initExtra}

      PS1="[osh] $PS1"
    '';
  };

  programs = {
    autojump.enable = isLinux;
    bat.enable = true;
    direnv.enable = true;
    direnv.enableBashIntegration = true;
    emacs.enable = true;
    emacs.package = pkgs.my-emacs;
    git.enable = true;
    git.extraConfig = gitconfig;
    gpg.enable = true;
    htop.enable = isDarwin;
    jq.enable = true;
    netclip.enable = isLinux;
    skim.defaultOptions = [ "-m" "--color=bw" "--layout=reverse" ];
    skim.enable = true;
    tmux.enable = true;
    tmux.package = pkgs.tmux;
  };

  services.netclip.enable = isDarwin;

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
      KeepAlive = true;
      ProgramArguments = [
        "/bin/sh"
        "-c"
        "/bin/wait4path ${pkgs.my-emacs}/bin/emacs &amp;&amp; exec ${pkgs.my-emacs}/bin/emacs --fg-daemon=${config.home.username}"
      ];
    };
  };
}
