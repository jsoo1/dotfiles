{ lib, pkgs, config, ... }:
let
  svcCfg = config.services.soclip;

  prgCfg = config.programs.soclip;

  soclip = pkgs.symlinkJoin {
    name = "soclip";
    paths = [
      (pkgs.writeShellApplication {
        name = "socopy";
        runtimeInputs = [ pkgs.socat ];
        text = ''
          socat -u - UNIX-CLIENT:${config.xdg.stateHome}/${svcCfg.socketPath}-copy
        '';
      })
      (pkgs.writeShellApplication {
        name = "sopaste";
        runtimeInputs = [ pkgs.socat ];
        text = ''
          socat -u UNIX-CLIENT:${config.xdg.stateHome}/${svcCfg.socketPath}-paste -
        '';
      })
    ];
  };

  soclipd = pkgs.symlinkJoin {
    name = "soclipd";
    paths = [
      (pkgs.writeShellApplication {
        name = "soclipd-copy";
        runtimeInputs = [ pkgs.socat ];
        text = ''
          rm UNIX-LISTEN:${config.xdg.stateHome}/${svcCfg.socketPath}-copy || :
          socat -u UNIX-LISTEN:${config.xdg.stateHome}/${svcCfg.socketPath}-copy,fork EXEC:pbcopy
        '';
      })
      (pkgs.writeShellApplication {
        name = "soclipd-paste";
        runtimeInputs = [ pkgs.socat ];
        text = ''
          rm ${config.xdg.stateHome}/${svcCfg.socketPath}-paste || :
          socat UNIX-LISTEN:${config.xdg.stateHome}/${svcCfg.socketPath}-paste,fork EXEC:pbpaste
        '';
      })
    ];
  };
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

  config = {
    launchd.agents.soclip-copy = {
      enable = svcCfg.enable;
      config = {
        Program = "${soclipd}/bin/soclipd-copy";
        KeepAlive = true;
      };
    };

    launchd.agents.soclip-paste = {
      enable = svcCfg.enable;
      config = {
        Program = "${soclipd}/bin/soclipd-paste";
        KeepAlive = true;
      };
    };

    home.activation.soclip = lib.mkIf (svcCfg.enable || prgCfg.enable) (lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      $DRY_RUN_CMD mkdir -p $VERBOSE_ARG ${builtins.dirOf "${config.xdg.stateHome}/${svcCfg.socketPath}"}
    '');

    home.packages = lib.mkIf prgCfg.enable [ soclip ];
  };
}
