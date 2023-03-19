{ lib, pkgs, config, ... }:
let
  svcCfg = config.services.soclip;

  prgCfg = config.programs.soclip;

  SOCLIP_SOCK_DIR = "${config.xdg.stateHome}/${svcCfg.socketDir}";
in
{
  options.services.soclip = {
    enable = lib.mkEnableOption "soclip";

    socketDir = lib.mkOption {
      type = lib.types.str;
      default = "soclip/socks";
      example = "my-sockets/soclip";
      description = ''
        Socket path for clipboard relative to $XDG_STATE_HOME.
      '';
    };
  };

  options.programs.soclip.enable =
    lib.mkEnableOption "soclip";

  config = {
    launchd.agents.soclipd-copy = {
      inherit (svcCfg) enable;
      config = {
        EnvironmentVariables = { inherit SOCLIP_SOCK_DIR; };
        ProgramArguments = [ "${pkgs.soclipd}/bin/soclipd" "copy" ];
        KeepAlive = true;
      };
    };

    launchd.agents.soclipd-paste = {
      inherit (svcCfg) enable;
      config = {
        EnvironmentVariables = { inherit SOCLIP_SOCK_DIR; };
        ProgramArguments = [ "${pkgs.soclipd}/bin/soclipd" "paste" ];
        KeepAlive = true;
      };
    };

    home.activation.soclip = lib.mkIf (svcCfg.enable || prgCfg.enable)
      (lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        $DRY_RUN_CMD mkdir -p $VERBOSE_ARG ${SOCLIP_SOCK_DIR}
      '');

    home.packages = lib.mkIf prgCfg.enable [
      (pkgs.runCommand "soclip-wrapped" { nativeBuildInputs = [ pkgs.makeWrapper ]; } ''
        makeWrapper ${pkgs.soclip}/bin/socopy $out/bin/socopy \
          --set SOCLIP_SOCK_DIR "${SOCLIP_SOCK_DIR}"

        makeWrapper ${pkgs.soclip}/bin/sopaste $out/bin/sopaste \
          --set SOCLIP_SOCK_DIR "${SOCLIP_SOCK_DIR}"
      '')
    ];
  };
}
