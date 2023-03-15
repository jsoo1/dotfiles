{ lib, ... }: {
  options.pasteSock = lib.mkOption {
    type = lib.types.nullOr lib.types.str;
    default = "paste-listener/sock";
    example = "paste-sock";
    description = ''
      Socket path for clipboard relative to $XDG_STATE_HOME.
    '';
  };
}
