# To update: ./update

{ pkgs ? import ./nixpkgs.nix }:

let
  python = import ./requirements.nix { inherit pkgs; };
  pyPkgs = python.packages;
  versionInfo = import ./version.nix;

in python.mkDerivation rec {
  name = "sumall-cli-${version}";
  version = versionInfo.rev;

  src = pkgs.fetchFromGitHub {
    owner = "SumAll";
    repo = "sumall-cli";
    private = true;
    inherit (versionInfo) rev sha256;
  };

  buildInputs = with pkgs; [ libffi openssl ];

  propagatedBuildInputs = [
    pyPkgs.ansicolors
    pyPkgs.argcomplete
    pyPkgs.boto
    pyPkgs.boto3
    pyPkgs.botocore
    pyPkgs.colorama
    pyPkgs.credstash
    pyPkgs.html5lib
    pyPkgs.Jinja2
    pyPkgs.kubernetes
    pyPkgs.marshmallow
    pyPkgs.progressbar
    pyPkgs.PyYAML
    pyPkgs.redis
    pyPkgs.requests
    pyPkgs.retrying
    pyPkgs.semantic-version
    pyPkgs.urllib3
  ];

  preConfigure = ''
    sed -i '/^argparse/d' requirements.txt
  '';

  postFixup = ''
    makeWrapper \
      ${pyPkgs.argcomplete}/bin/register-python-argcomplete \
      $out/bin/register-sumall-argcomplete \
      --argv0 '$0' --add-flags sumall
  '';

  checkInputs = [ pyPkgs.pytest ];

  checkPhase = "pytest -v";
}
