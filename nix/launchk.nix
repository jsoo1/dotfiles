{ lib, darwin, ncurses, llvmPackages, fetchFromGitHub }:
let
  pname = "launchk";

  version = "0.2.0";

  owner = "mach-kernel";

  inherit (darwin.apple_sdk_11_0)
    rustPlatform
    frameworks
    llvmPackages_16
    MacOSX-SDK
    ;

  self = rustPlatform.buildRustPackage {
    inherit pname version;

    src = fetchFromGitHub {
      inherit owner;
      repo = pname;
      rev = "launchk-${version}";
      sha256 = "sha256-CuN06Z2ARHGPxzFie3pwRfeuEgrUQHlsLw7iJL/KfBg=";
    };

    cargoSha256 = "sha256-APfaLqB3onYxf4jZnajsRvGWe9FpZOcK9i1FElBJqOM=";

    patches = [
      ./launchk-include-dirs.patch
      ./launchk-no-git-version.patch
    ];

    buildInputs = [
      ncurses
    ] ++ (with frameworks; [
      CoreFoundation
      CoreServices
    ]);

    LIBCLANG_PATH = "${llvmPackages.libclang.lib}/lib";

    NIX_LDFLAGS = [ "-L${MacOSX-SDK}/usr/lib" ];

    apple_sdk = MacOSX-SDK;

    postPatch = ''
      substituteAllInPlace xpc-sys/build.rs
      substituteAllInPlace launchk/src/main.rs
    '';

    meta = {
      description = "Cursive TUI that queries XPC to help manage launchd jobs";
      homepage = "https://github.com/${owner}/${pname}";
      platforms = lib.platforms.darwin;
      maintainers = [ lib.maintainers.jsoo1 ];
    };
  };

in
self.overrideAttrs (_: {
  passthru.shell = self.overrideAttrs (o: {
    nativeBuildInputs = (o.nativeBuildInputs or [ ]) ++ [
      llvmPackages_16.lldb
      self
    ];
  });
})
