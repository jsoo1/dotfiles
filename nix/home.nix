{ config, lib, ... }:
let
  pkgs = import ./pin.nix;

  isDarwin = builtins.currentSystem == "x86_64-darwin";

  home = config.home.homeDirectory;
  username = config.home.username;
  dotfiles = "${home}/dotfiles";

  feeds = {
    ".emacs.d/feeds".recursive = true;
    ".emacs.d/feeds".source = "${dotfiles}/rss";
  };

  ssh-auth-sock = "${home}/.ssh/auth_sock";

  em =
    "emacsclient -t ${if !isDarwin then "--socket-name=${username}" else ""}";
  tm = dir:
    let emacs-cmd = if isDarwin then "emacs" else em;
    in ''
      tmux new-session -A -s $(basename "${dir}" | tr '.' '-') -c "${dir}" ${emacs-cmd} ${dir}'';

  shellAliases = {
    inherit em;
    tm = tm "$PWD";
    tml = "tmux list-sessions";
    tma = "tmux attach-session -t";
    lsa = "ls -lsa";
    vi = "nvim";
    pb = "curl -F c=@- pb";
  };

  skim-cmds = {
    projects = ''
      fd '.git$' ~ -t d -H -x dirname | sk | tr -d '\n'
    '';
    files = "fd '.*' '.' --hidden -E '.git*' | sk";
    history = ''
      history | sk | awk '{$1=""}1'
    '';
  };

  services.gpg-agent.enable = true;
  services.gpg-agent.extraConfig = "allow-emacs-pinentry";

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
  home = lib.optionalAttrs isDarwin { inherit activation; } // {
    extraOutputsToInstall = [ "doc" ];

    packages = let
      inherit (pkgs)
        dogdns fd gawk ghcid git haskell-language-server iosevka nix-diff
        nix-prefetch nixfmt rage restream ripgrep rnix-lsp watch;
      fonts = [ iosevka ];
      haskell-utilities = [ ghcid haskell-language-server ];
      nix-utilities = [ nixfmt nix-diff nix-prefetch rnix-lsp ];
      remarkable-utilities = [ restream ];
      shell-utilities = [ dogdns fd gawk git rage ripgrep watch ];
    in builtins.concatLists [
      haskell-utilities
      nix-utilities
      shell-utilities
      (lib.optionals isDarwin (fonts ++ remarkable-utilities))
    ];

    file = lib.optionalAttrs isDarwin feeds // {
      ".ghci".source = "${dotfiles}/ghci/.ghci";
      ".haskeline".source = "${dotfiles}/ghci/.haskeline";
      ".psqlrc".source = "${dotfiles}/psql/.psqlrc";
      ".vimrc".source = "${dotfiles}/minimal/.vimrc";
      ".tmux.conf".source = "${dotfiles}/minimal/.tmux.conf";
    };
  };

  programs = {
    bat.enable = true;
    direnv.enable = true;
    emacs.enable = true;
    emacs.package = pkgs.my-emacs;
    gpg.enable = true;
    htop.enable = true;
    jq.enable = true;
    neovim.enable = true;
    skim.defaultOptions = [ "-m" "--color=bw" ];
    skim.enable = true;
    tmux.enable = true;

    bash = {
      enable = !isDarwin;
      inherit shellAliases;
      initExtra = let
        set-prompt-to = cmd:
          ''
            " \C-b\C-k \C-u`${cmd}`\e\C-e\er\C-a\C-y\C-h\C-e\e \C-y\ey\C-x\C-x\C-f"'';
        bind-key = kbd: val: ''
          bind -m emacs-standard '"\${kbd}": ${val}'
        '';
      in ''
        # Keybindings
        skim-projects () {
          local proj="$(${skim-cmds.projects})"
          [ "" != "$proj" ] && echo ${tm "$proj"}
        }
        skim-files () {
          echo $(${skim-cmds.files})
        }
        skim-history () {
          echo $(${skim-cmds.history})
        }
        ${bind-key "C-o" (set-prompt-to "skim-projects")}
        ${bind-key "C-t" (set-prompt-to "skim-files")}
        ${bind-key "C-r" (set-prompt-to "skim-history")}

        # Git support
        [ -n "$SSH_AUTH_SOCK" ] && ln -sf "$SSH_AUTH_SOCK" ${ssh-auth-sock}
      '';
    };

    zsh = {
      enable = isDarwin;
      inherit shellAliases;
      initExtra = let
        mkWidget = name: body: ''
          __${name} () {
            ${body}
          }
          zle -N ${name} __${name}
        '';
      in ''
        restart-nix-daemon () {
            sudo launchctl bootout system/org.nixos.nix-daemon \
                && sudo launchctl load /Library/LaunchDaemons/org.nixos.nix-daemon.plist
        }

        # ----- Keybindings -----
        ${mkWidget "skim_history" "LBUFFER=$LBUFFER$(${skim-cmds.history})"}
        ${mkWidget "skim_files" "LBUFFER=$LBUFFER$(${skim-cmds.files})"}
        ${mkWidget "tmux_projects" ''
          local proj="$(${skim-cmds.projects})"
          [ "" != "$proj" ] && LBUFFER="${tm "$proj"}"
        ''}
        bindkey '^r' skim_history
        bindkey '^t' skim_files
        bindkey '^o' tmux_projects
      '';
    };
  };
}
