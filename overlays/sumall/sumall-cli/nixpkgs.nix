# To update: ./update-nixpkgs REVISION

import (builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/be1461fc0ab29c45c69e7b2c2097c887750e4fe0.tar.gz";
  sha256 = "1fdlnfipa3qpyzqqqy3cf5zicgj6chwmd40yz8n60nil8qi6452w";
}) { config = {}; overlays = []; }
