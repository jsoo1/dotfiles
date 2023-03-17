{ lib, pkgs, config, ... }:
let
  cfg = config.services.soclip;

  soclipCfg = config.programs.soclip;
in
{
  options.services.soclip = {
    enable = lib.mkEnableOption "soclip";

    socketPath = lib.mkOption {
      type = lib.types.str;
      default = "soclip/sock";
      example = "paste-sock";
      description = ''
        Socket path for clipboard relative to $XDG_STATE_HOME.
      '';
    };
  };

  options.programs.soclip.enable =
    lib.mkEnableOption "soclip";

  config = lib.mkMerge [
    {
      launchd.agents.soclip-copy.config = {
        Program = let soclipd-copy = pkgs.writeShellApplication {
          name = "soclipd-copy";
          runtimeInputs = [ pkgs.socat ];
          text = ''
            rm UNIX-LISTEN:${config.xdg.stateHome}/${cfg.socketPath}-copy || :
            socat -u UNIX-LISTEN:${config.xdg.stateHome}/${cfg.socketPath}-copy,fork EXEC:pbcopy
          '';
        }; in "${soclipd-copy}/bin/soclipd-copy";
        KeepAlive = true;
      };

      launchd.agents.soclip-paste.config = {
        Program = let soclipd-paste = pkgs.writeShellApplication {
          name = "soclipd-paste";
          runtimeInputs = [ pkgs.socat ];
          text = ''
            rm ${config.xdg.stateHome}/${cfg.socketPath}-paste || :
            socat UNIX-LISTEN:${config.xdg.stateHome}/${cfg.socketPath}-paste EXEC:pbpaste
          '';
        }; in "${soclipd-paste}/bin/soclipd-paste";
        KeepAlive = true;
        ThrottleInterval = 0;
      };
    }

    (lib.mkIf cfg.enable {
      launchd.agents.soclip-copy.enable = true;

      launchd.agents.soclip-paste.enable = true;

      home.activation.soclip = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        $DRY_RUN_CMD mkdir -p $VERBOSE_ARG ${builtins.dirOf "${config.xdg.stateHome}/${cfg.socketPath}"}
      '';
    })

    (lib.mkIf soclipCfg.enable {
      home.packages = [
        (pkgs.writeShellApplication {
          name = "socopy";
          runtimeInputs = [ pkgs.socat ];
          text = ''
            socat -u - UNIX-CLIENT:${config.xdg.stateHome}/${cfg.socketPath}-copy
          '';
        })
        (pkgs.writeShellApplication {
          name = "sopaste";
          runtimeInputs = [ pkgs.socat ];
          text = ''
            socat -u UNIX-CLIENT:${config.xdg.stateHome}/${cfg.socketPath}-paste -
          '';
        })
      ];

      home.activation.soclip = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        $DRY_RUN_CMD mkdir -p $VERBOSE_ARG ${builtins.dirOf "${config.xdg.stateHome}/${cfg.socketPath}"}
      '';
    })
  ];
}
