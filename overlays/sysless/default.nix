{ stdenv
, fetchFromGitHub
, less
}:

stdenv.mkDerivation {
  name = "sysless";

  src = fetchFromGitHub {
    owner = "ivanbrennan";
    repo = "sysless";
    rev = "4bf79562a6ff6563ceab7baf5c811a8e3553561c";
    sha256 = "1mdnig5crf2cgbz1m94qak7l8w0ydc55cw26hnm4a28nj3ikqxf6";
  };

  buildInputs = [ less ];
  builder = ./builder.sh;

  meta = {
    description = "Key bindings for Less";
  };
}
