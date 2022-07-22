[
  (self: super: {
    procps = super.procps.overrideAttrs (o: {
      configureFlags = builtins.filter (flag: flag != "--disable-modern-top")
        (o.configureFlags or [ ]);
    });
  })
]
