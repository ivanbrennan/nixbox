let
  url = https://github.com/SumAll/nixpkgs-sumall/archive/master.tar.gz;
in import (builtins.fetchTarball url)
