pkgs:

let
  build = name: value: pkgs.vimUtils.buildVimPlugin {
    name = value.name;
    src = pkgs.fetchFromGitHub value.src;
  };

  plugins = builtins.fromJSON (builtins.readFile ./plugins.json);

in pkgs.lib.mapAttrs build plugins
