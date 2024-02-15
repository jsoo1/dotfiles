[
  (final: prev: {
    defaultShell = final.lib.flip final.callPackage { } ({ mkShell, git }: mkShell {
      nativeBuildInputs = [ git ];
    });
  })
]
