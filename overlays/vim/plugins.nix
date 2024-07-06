pkgs:

let
  build = name: value: pkgs.vimUtils.buildVimPlugin {
    name = value.name;
    src = pkgs.fetchFromGitHub value.src;
  };

  plugins = builtins.fromJSON (builtins.readFile ./plugins.json);

  # TODO: Why doesn't pkgs.vimPlugins.lush-nvim work as expected? It doesn't
  # provide a plugin directory. It seems that might be due to the luarocks
  # package by the same name taking precidence? Also, it only seems to work
  # (both the original and this version) when included in "start" plugins. If
  # included in "opt" instead, we can load it with :packadd! but the commands it
  # should provide are not actually made available.

in pkgs.lib.mapAttrs build plugins // {
  ncore-plugin = pkgs.vimUtils.buildVimPlugin {
    name = "ncore-plugin";
    src = ./ncore-plugin;
  };
}
