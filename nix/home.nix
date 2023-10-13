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

      # direnv breaks in osh because of
      # https://github.com/oilshell/oil/issues/1607
      _direnv_hook() {
        local previous_exit_status=$?;
        eval "$("/Users/johh.soo/.nix-profile/bin/direnv" export bash)";
        return $previous_exit_status;
      };
      if ! [[ "''${PROMPT_COMMAND:-}" =~ _direnv_hook ]]; then
        PROMPT_COMMAND="_direnv_hook''${PROMPT_COMMAND:+;$PROMPT_COMMAND}"
      fi
    '';
  };

  programs = {
    autojump.enable = isLinux;
    bat.enable = true;
    direnv.enable = true;
    direnv.enableBashIntegration = isLinux;
    emacs.enable = true;
    emacs.package = pkgs.my-emacs;
    git.enable = true;
    git.package = pkgs.gitFull;
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
      KeepAlive = true;
      ProgramArguments = [
        "/bin/sh"
        "-c"
        "/bin/wait4path ${pkgs.my-emacs}/bin/emacs &amp;&amp; exec ${pkgs.my-emacs}/bin/emacs --fg-daemon=${config.home.username}"
      ];
    };
  };
}
