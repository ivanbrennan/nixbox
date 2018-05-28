{ pkgs }:

let
  python = import ./requirements.nix { inherit pkgs; };
in

python.mkDerivation {
  name = "sumall-cli";
  version = "7.1.0";

  src = pkgs.fetchgitPrivate {
    url = "git@github.com:SumAll/sumall-cli.git";
    rev = "3d6fb23c4390f1e6bac3d28200d20166ca7ec292";
    sha256 = "18m53zcjbxadvrg5ggk0n9jdmb6v24rjn1wkqw54ximg8pf6gnml";
  };

  propagatedBuildInputs = [
    python.packages."Jinja2"
    python.packages."PyYAML"
    python.packages."ansicolors"
    python.packages."argcomplete"
    python.packages."argparse"
    python.packages."boto"
    python.packages."boto3"
    python.packages."botocore"
    python.packages."colorama"
    python.packages."credstash"
    python.packages."html5lib"
    python.packages."kubernetes"
    python.packages."marshmallow"
    python.packages."progressbar"
    python.packages."redis"
    python.packages."requests"
    python.packages."retrying"
    python.packages."semantic-version"
    python.packages."urllib3"
  ];

  checkInputs = [ python.packages."pytest" ];

  checkPhase = "pytest -v";
}
