# To update: ./update-nixpkgs REVISION

import (builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/5da85431fb1df4fb3ac36730b2591ccc9bdf5c21.tar.gz";
  sha256 = "0pc15wh5al9dmhcj29gwqir3wzpyk2nrplibr5xjk2bdvw6sv6c1";
}) { config = {}; overlays = []; }
