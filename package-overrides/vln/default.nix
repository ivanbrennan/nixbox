{ stdenv
, fetchFromGitHub
, stow
}:

stdenv.mkDerivation {
  name = "vln";

  src = fetchFromGitHub {
    owner = "ivanbrennan";
    repo = "vln";
    rev = "c75b9e752cd11930112d008f8178308e4f616b82";
    sha256 = "179nv6iiwjycv401xb5b8jl7gv1k4rr881jzmcf4d0ywfc2b4ljr";
  };

  propagatedBuildInputs = [ stow ];
  builder = ./builder.sh;

  meta = {
    description = "Manage ~/.vim symlinks";
  };
}
