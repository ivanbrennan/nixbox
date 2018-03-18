{ stdenv
, fetchFromGitHub
}:

stdenv.mkDerivation {
  name = "dotvim";

  src = fetchFromGitHub {
    owner = "ivanbrennan";
    repo = "dotvim";
    rev = "1fe95b456e92df175656a6420e03e5de4cc79f9d";
    sha256 = "0kz580gadqdwq8mc5ayrwq8a4amksmbcqk6pkxkc5b16kny72hws";
  };

  builder = ./builder.sh;

  meta = {
    description = "A minimal vimrc.";
  };
}
