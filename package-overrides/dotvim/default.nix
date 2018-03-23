{ stdenv
, fetchFromGitHub
}:

stdenv.mkDerivation {
  name = "dotvim";

  src = fetchFromGitHub {
    owner = "ivanbrennan";
    repo = "dotvim";
    rev = "ef4b550fb3e668bea2ba38b09d46296847ca482f";
    sha256 = "1izxx6amm3v8f0z61ghbcyih5k1pgaadabn4ny1v9yvrbs1dz7sv";
  };

  builder = ./builder.sh;

  meta = {
    description = "A minimal vimrc.";
  };
}
