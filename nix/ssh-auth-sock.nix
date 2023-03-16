{ config, lib, ... }: {
  options.ssh-auth-sock = lib.mkOption {
    type = lib.types.path;
    example = lib.literalExpression ''''${config.xdg.stateDir}/ssh/auth_sock'';
    description = ''
      Where to put the ssh auth_sock.

      Default: ''${config.home.homeDirectory}/.ssh/auth_sock
    '';
  };

  config.ssh-auth-sock = lib.mkDefault
    "${config.home.homeDirectory}/.ssh/auth_sock";
}
