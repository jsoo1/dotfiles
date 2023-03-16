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
      launchd.agents.soclip.config = {
        Program = let soclipd = pkgs.writeShellApplication {
          name = "soclipd";
          runtimeInputs = [ pkgs.socat ];
          text = ''
            pbpaste | socat -ddd UNIX-LISTEN:${config.xdg.stateHome}/${cfg.socketPath},fork EXEC:pbcopy
          '';
        }; in "${soclipd}/bin/soclipd";
        KeepAlive = true;
        StandardErrorPath =
          "${config.xdg.stateHome}/soclip/soclip.log";
      };
    }

    (lib.mkIf cfg.enable {
      launchd.agents.soclip.enable = true;

      home.activation.soclip = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        $DRY_RUN_CMD mkdir -p $VERBOSE_ARG ${builtins.dirOf "${config.xdg.stateHome}/${cfg.socketPath}"}
      '';
    })

    (lib.mkIf soclipCfg.enable {
      home.packages = [
        (pkgs.writeShellApplication {
          name = "soclip";
          runtimeInputs = [ pkgs.socat ];
          text = ''
            socat -u - UNIX-CLIENT:${config.xdg.stateHome}/${cfg.socketPath} 
          '';
        })
      ];

      home.activation.soclip = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        $DRY_RUN_CMD mkdir -p $VERBOSE_ARG ${builtins.dirOf "${config.xdg.stateHome}/${cfg.socketPath}"}
      '';
    })
  ];
}
