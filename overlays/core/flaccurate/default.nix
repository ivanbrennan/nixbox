{ fetchFromGitHub
, perlPackages
, buildPerlPackage
}:

buildPerlPackage rec {
  name = "flaccurate";

  src = fetchFromGitHub {
    owner = "ivanbrennan";
    repo = "flaccurate";
    rev = "294a259eee3b86ff90add954584f7cedff15838a";
    sha256 = "0l1wzl8x0yyji21d6nvs1kcl6qlsy2l87h0hcb6qvjrcdzmcy82k";
  };

  doCheck = false;

  outputs = [ "out" ];

  propagatedBuildInputs = with perlPackages; [ LWP ];

  meta = {
    description = "Check FLAC files against AccurateRips";
  };
}
