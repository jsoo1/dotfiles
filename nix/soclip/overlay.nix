final: prev: {
  soclip = prev.symlinkJoin {
    name = "soclip";
    paths = [
      (prev.writeShellApplication {
        name = "socopy";
        runtimeInputs = [ prev.socat ];
        text = ''
          socat -u - "UNIX-CLIENT:''${SOCLIP_SOCK_DIR}/copy"
        '';
      })
      (prev.writeShellApplication {
        name = "sopaste";
        runtimeInputs = [ prev.socat ];
        text = ''
          socat -u "UNIX-CLIENT:''${SOCLIP_SOCK_DIR}/paste" -
        '';
      })
    ];
  };

  soclipd = prev.writeShellApplication {
    name = "soclipd";
    runtimeInputs = [ prev.socat ];
    text = ''
      rm "UNIX-LISTEN:''${SOCLIP_SOCK_DIR}/$1" || :
      socat "UNIX-LISTEN:''${SOCLIP_SOCK_DIR}/$1,fork" "EXEC:pb$1"
    '';
  };
}
