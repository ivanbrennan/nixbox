# To update: ./update-nixpkgs.sh REVISION

import (builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/dae9cf6106da19f79a39714f183ed253c62b32c5.tar.gz";
  sha256 = "0r3c00m96ldb9z81ay7vj8gnpk4bf8gjcdiad7mgxvwxr9ndskjx";
}) { config = {}; overlays = []; }
