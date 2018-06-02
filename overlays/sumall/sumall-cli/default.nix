# To update: ./update-sumall VERSION

{ pkgs ? import ./nixpkgs.nix }:

with pkgs;

let
  depName = dep: (builtins.parseDrvName dep.name).name;
  depMatch = dep: name: stdenv.lib.hasSuffix ("-" + name) (depName dep);

  removeDependencies = names: deps:
    builtins.filter
    (dep: builtins.all (name: !depMatch dep name) names)
    deps;

  python = python2.override {
    packageOverrides = self: super: {

      boto3 = super.boto3.overridePythonAttrs (old: rec {
        version = "1.6.7";
        src = fetchFromGitHub {
          owner  = "boto";
          repo   = "boto3";
          rev    = version;
          sha256 = "1758h04rzc0l9csvqw0a0r1wlpm2l1xd1qvvlllj75hm1ixrc76h";
        };
      });

      botocore = super.botocore.overridePythonAttrs (old: rec {
        version = "1.9.7";
        src = old.src.override {
          inherit version;
          sha256 = "1hbpr47r3h17fhz33h2mdgi3zk9f9afk2g5ly1yin3d5ym5rk507";
        };
      });

      credstash = super.credstash.overridePythonAttrs (old: rec {
        version = "1.13.4";
        src = old.src.override {
          inherit version;
          sha256 = "10czhy2yjf4kdm7sky0iqhksykx612fz1vlaqxhc5pj3c4xc0v37";
        };
        patches = [ ];
      });

      cryptography = super.cryptography.overridePythonAttrs (old: rec {
        version = "2.0.3";
        src = old.src.override {
          inherit version;
          sha256 = "d04bb2425086c3fe86f7bc48915290b13e798497839fbb18ab7f6dffcf98cc3a";
        };
      });

      kubernetes = super.kubernetes.overridePythonAttrs (old: rec {
        version = "6.0.0";
        src = old.src.override {
          inherit version;
          sha256 = "128wfhmrj57ch8ksj3767nlfjj3r90ilfjm1d7dhjlwjpm5anw5k";
        };
      });

      redis = super.redis.overridePythonAttrs (old: rec {
        version = "2.10.5";
        src = old.src.override {
          inherit version;
          sha256 = "0csmrkxb29x7xs9b51zplwkkq2hwnbh9jns1g85dykn5rxmaxysx";
        };
      });

      requests = super.requests.overridePythonAttrs (old: rec {
        version = "2.14.2";
        src = old.src.override {
          inherit version;
          sha256 = "0lyi82a0ijs1m7k9w1mqwbmq1qjsac35fazx7xqyh8ws76xanx52";
        };
      });

      urllib3 = super.urllib3.overridePythonAttrs (old: rec {
        version = "1.20";
        name = "urllib3-1.20";
        src = old.src.override {
          inherit version;
          sha256 = "0bx76if7shzlyykmaj4fhjkir5bswc4fdx5r4q0lrn3q51p2pvwp";
        };
        propagatedBuildInputs =
          removeDependencies [ "pyOpenSSL" ] old.propagatedBuildInputs;
      });

    };
  };

  sumallVersion = import ./version.nix;

in with python.pkgs; buildPythonApplication rec {
  name = "sumall-cli-${version}";
  version = sumallVersion.rev;

  src = fetchFromGitHub {
    owner = "SumAll";
    repo = "sumall-cli";
    private = true;
    inherit (sumallVersion) rev sha256;
  };

  buildInputs = [ libffi openssl ];

  propagatedBuildInputs = [
    ansicolors
    argcomplete
    boto
    boto3
    botocore
    colorama
    credstash
    html5lib
    jinja2
    kubernetes
    marshmallow
    progressbar
    pyyaml
    redis
    requests
    retrying
    semantic-version
    urllib3
  ];

  preConfigure = ''
    sed -i '/^argparse/d' requirements.txt
  '';

  postFixup = ''
    makeWrapper \
      ${argcomplete}/bin/register-python-argcomplete \
      $out/bin/register-sumall-argcomplete \
      --argv0 '$0' --add-flags sumall
  '';

  checkInputs = [ pytest ];

  checkPhase = "pytest -v";
}
