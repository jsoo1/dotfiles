{ pkgs, dotfiles, config, lib, ... }:
let
  home = config.home.homeDirectory;
  username = config.home.username;

  env = import ./env.nix { inherit pkgs; };

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
    core = { editor = "${pkgs.neovim}/bin/nvim"; };
    diff = { renames = true; };
    github = { user = "jsoo1"; };
    commit = { gpgsign = true; };
    branch = { autoSetupMerge = false; };

  };

  linux-only = {
    inherit systemd;
    services.gpg-agent.enable = true;
    home.packages = env.shell-utilities ++ [
      pkgs.iosevka
      pkgs.home-manager
    ];
  };

  darwin-only.home.file = feeds;


in
{
  imports = [ ./options.nix ];

  config =
    lib.mkMerge [
      (lib.mkIf (!pkgs.stdenv.hostPlatform.isDarwin) linux-only)
      (lib.mkIf pkgs.stdenv.hostPlatform.isDarwin darwin-only)
      (rec {
        home = {
          extraOutputsToInstall = [ "doc" "nc" ];

          stateVersion = "22.05";

          packages = env.user;

          file = {
            ".ghci".source = "${dotfiles}/ghci/.ghci";
            ".haskeline".source = "${dotfiles}/ghci/.haskeline";
            ".psqlrc".source = "${dotfiles}/psql/.psqlrc";
            ".vimrc".source = "${dotfiles}/minimal/.vimrc";
          };

          activation.emacs = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
            $DRY_RUN_CMD ln -sfv $VERBOSE_ARG ${config.home.homeDirectory}/{dotfiles/nix,.emacs.d}/init.el
          '';
        };

        xdg.configFile = {
          "nvim/init.vim".source = "${dotfiles}/minimal/.vimrc";
          "tmux/tmux.conf".source = pkgs.runCommand "tmux.conf" ''
            cat <<EOF > $out
            $(cat "${dotfiles}/nix/.tmux.conf")

            # clipboard for local/remotes
            set -s copy-command '${
              if pkgs.stdenv.hostPlatform.isDarwin
              then "pbcopy"
              else "${pkgs.socat}/bin/socat -u - UNIX-CLIENT:${config.home.xdg.stateDir}/${config.pasteSocket}"}'
            EOF
          '';
          "procps/toprc".source = "${dotfiles}/top/toprc";
          "oil/oshrc".text = ''
            ${lib.optionalString pkgs.stdenv.hostPlatform.isDarwin ''
               # Avoids errors in /etc/bashrc_Apple_Terminal (unused anyways)
               TERM_PROGRAM_OLD="$TERM_PROGRAM"
               TERM_PROGRAM=junk
               source /etc/bashrc
               TERM_PROGRAM="$TERM_PROGRAM_OLD"
            ''}

            ${lib.concatStringsSep "\n" (lib.mapAttrsToList (var: val: ''export ${var}="${val}"'')
              programs.bash.sessionVariables)}

            ${lib.concatStringsSep "\n" (lib.mapAttrsToList (var: val: "alias ${var}=${lib.escapeShellArg val}")
              programs.bash.shellAliases)}

            export SKIM_DEFAULT_OPTIONS="${lib.concatStringsSep " " programs.skim.defaultOptions}"

            alias tmn='eval "$(tmux-projects)"'

            {programs.bash.initExtra}

            PS1="[osh] $PS1"
          '';
        };

        programs = {
          autojump.enable = pkgs.stdenv.hostPlatform.isLinux;
          bash = import ./bash.nix { inherit config lib ssh-auth-sock pkgs; };
          bat.enable = true;
          direnv.enable = true;
          direnv.enableBashIntegration = true;
          emacs.enable = true;
          emacs.package = pkgs.my-emacs;
          git.enable = true;
          git.extraConfig = gitconfig;
          gpg.enable = true;
          htop.enable = pkgs.stdenv.hostPlatform.isDarwin;
          jq.enable = true;
          skim.defaultOptions = [ "-m" "--color=bw" "--layout=reverse" ];
          skim.enable = true;
          tmux.enable = true;
          tmux.package = pkgs.tmux;
        };
      })
    ];

}
