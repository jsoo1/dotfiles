{ lib, pkgs, config, ... }:
let
  cfg = config.services.netclip;

  netclipCfg = config.programs.netclip;
in
{
  options.services.netclip = {
    enable = lib.mkEnableOption "netclip";

    socketPath = lib.mkOption {
      type = lib.types.str;
      default = "netclip/sock";
      example = "paste-sock";
      description = ''
        Socket path for clipboard relative to $XDG_STATE_HOME.
      '';
    };
  };

  options.programs.netclip.enable =
    lib.mkEnableOption "netclip";

  config = lib.mkMerge [
    {
      launchd.agents.netclip.config = {
        Program = let netclipd = pkgs.writeShellApplication {
          name = "netclipd";
          runtimeInputs = [ pkgs.socat ];
          text = ''
            socat -u UNIX-LISTEN:${config.xdg.stateHome}/${cfg.socketPath},fork - \
              | pbcopy
          '';
        }; in "${netclipd}/bin/netclipd";
        KeepAlive = true;
      };
    }

    (lib.mkIf cfg.enable { launchd.agents.netclip.enable = true; })

    (lib.mkIf netclipCfg.enable {
      home.packages = [
        (pkgs.writeShellApplication {
          name = "netclip";
          runtimeInputs = [ pkgs.socat ];
          text = ''
            socat -u - UNIX-CLIENT:${config.xdg.stateHome}/${cfg.socketPath} 
          '';
        })
      ];

      home.activation.netclip = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        $DRY_RUN_CMD mkdir -p $VERBOSE_ARG ${builtins.dirOf "${config.xdg.stateHome}/${cfg.socketPath}"}
      '';
    })
  ];
}
