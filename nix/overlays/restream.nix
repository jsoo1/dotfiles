[
  (_: super: {
    restream = super.restream.overrideAttrs (_: {
      installPhase = ''
        runHook preInstall

        install -D restream.arm.static $out/libexec/restream.arm.static
        install -D reStream.sh $out/bin/restream

        runHook postInstall
      '';
      patches = [ ./restream-invert.patch ];
    });
  })
]
