{ fetchFromGitHub
, rustPlatform
}:

rustPlatform.buildRustPackage rec {
  pname = "dutree";
  version = "unstable-2020-06-23";

  src = fetchFromGitHub {
    owner = "nachoparker";
    repo = "dutree";
    rev = "83fc255965fb46c913a1c9b8be95c8b8e251c976";
    sha256 = "1sar1lv95z3apnpz2jry9ax7cvv64vj07ac84aqavwzc17szs97q";
  };

  cargoSha256 = "1nqgl8r4wjby8ckxflqm5pycaqb8813hq5x7dq7mqnpiqbd3p5zd";
}
