let
  self =
    {
      stdenv,
      fetchFromGitHub,
      mitm-cache,
      gradle,
      makeWrapper,
      jre8,
    }:

    stdenv.mkDerivation {
      pname = "groovy-language-server";
      version = "0.1.0";
      src = fetchFromGitHub {
        owner = "GroovyLanguageServer";
        repo = "groovy-language-server";
        rev = "cff39b87ffb3eb0dfc483ba727914824e871247e";
        hash = "sha256-bFc9JjAPeRBvFxKLR0L1km9KSE+8yCjSotnBW56V75g=";
      };

      __noChroot = true;

      nativeBuildInputs = [
        gradle
        mitm-cache
        makeWrapper
      ];

      gradleBuildTask = "build";

      preBuild = ''
        tmp_flags=()
        for flag in "''${gradleFlagsArray[@]}"; do
          if [[ "$flag" != --offline ]]; then
            tmp_flags+=("$flag")
          fi
        done
        gradleFlagsArray=(''${tmp_flags[@]})
      '';

      installPhase = ''
        mkdir -p $out/share/java/

        cp build/libs/*-all.jar \
          $out/share/java/groovy-language-server-all.jar

        makeWrapper ${jre8}/bin/java $out/bin/groovyls \
          --add-flags "-jar $out/share/java/groovy-language-server-all.jar"
      '';

      meta.mainProgram = "groovyls";
    };
in

[ (final: prev: { groovy-language-server = final.callPackage self { }; }) ]
