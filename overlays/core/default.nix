self: super: {
  abcde = super.abcde.overrideAttrs (old: rec {
    configurePhase = old.configurePhase + ''
      substituteInPlace "abcde" \
        --replace "$out/etc/abcde.conf" "${self.etcdots}/etc/abcde.conf"
    '';
  });

  credstash = with super.python3Packages; toPythonApplication (
    credstash.overridePythonAttrs (old: rec {
      postInstall = "rm $out/bin/credstash.py";
    })
  );

  etcdots = super.callPackage ./etcdots { };

  flaccurate = super.callPackage ./flaccurate { };

  interactive-editor = super.callPackage ./interactive-editor { };

  linuxPackages = (super.linuxPackagesFor super.linuxPackages.kernel).extend (
    self': super': {
      i8042_debounce = super'.callPackage ./i8042-debounce { };
    }
  );

  nice-backgrounds = super.callPackage ./nice-backgrounds { };

  vln = super.callPackage ./vln { };
}
