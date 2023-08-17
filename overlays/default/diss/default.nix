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

  cargoSha256 = "sha256-GOJcJDstsTLeTa1jI2DXetkWAtDn8DiumBUlFll9Oeg=";

  cargoPatches = [
    ./Cargo.lock.patch
  ];

  meta = with lib; {
    description = "dtach-like program / crate in rust";
    homepage = "https://github.com/yazgoo/diss";
  };
}
