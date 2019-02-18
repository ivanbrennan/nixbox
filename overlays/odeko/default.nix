let
  url = https://github.com/OdekoTeam/nixpkgs-odeko/archive/master.tar.gz;
in import (builtins.fetchTarball url)
