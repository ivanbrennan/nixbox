{ lib
, fetchFromGitHub
, rustPlatform
}:

rustPlatform.buildRustPackage rec {
  pname = "diss";
  version = "0.2.6";

  src = fetchFromGitHub {
    owner = "ivanbrennan";
    repo = pname;
    rev = "79e3e479db285003b04d86db9e90876bfb02e4bb";
    sha256 = "sha256-BMFGVC3EHP+TEBSlw5NNrzZ0DKDfblixCB9Bs8lFOlY=";
  };

  cargoHash = "sha256-h7KBL+RieCShfqBGl3htZdOgW/zZ9IyeKFSpINFkxS8=";

  cargoPatches = [
    ./Cargo.lock.patch
  ];

  meta = with lib; {
    description = "dtach-like program / crate in rust";
    homepage = "https://github.com/yazgoo/diss";
  };
}
