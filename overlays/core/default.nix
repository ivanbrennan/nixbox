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

  kubernetes-helm-2_11 =
    (import (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/a071bfa7e7bbd62e1b43830e5e79d8b36afe5fa6.tar.gz";
      sha256 = "0yl2bsan5x69a7z6r1fb8zlv4zpibq67pb2x45jjp7wx42ssdkq2";
    }) { }).kubernetes-helm;

  stack-1_9_3 =
    (import (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/bc94dcf500286495e3c478a9f9322debc94c4304.tar.gz";
      sha256 = "1siqklf863181fqk19d0x5cd0xzxf1w0zh08lv0l0dmjc8xic64a";
    }) { }).stack;

  linuxPackages = (super.linuxPackagesFor super.linuxPackages.kernel).extend (
    self': super': {
      i8042_debounce = super'.callPackage ./i8042-debounce { };
    }
  );

  nice-backgrounds = super.callPackage ./nice-backgrounds { };

  interception-tools-plugins = super.interception-tools-plugins // {
    space2meta = super.callPackage ./space2meta { };
  };

  vln = super.callPackage ./vln { };
}
