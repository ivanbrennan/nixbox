pkgs:

let
  customPlugins.coot = pkgs.vimUtils.buildVimPlugin {
    name = "coot";
    src = pkgs.fetchFromGitHub {
      owner = "ivanbrennan";
      repo = "coot";
      rev = "dbba4c34a20e1cebcd54e62bec0de455f5615adb";
      sha256 = "14h0r5hkwx0r5rhzc56sdnkdczzy2nrv8xhyj2hsgwbyaa3a5j16";
    };
  };
  vim = pkgs.vim_configurable.customize {
    name = "vim";
    vimrcConfig.customRC = import ./vimrc;
    vimrcConfig.vam.knownPlugins = pkgs.vimPlugins // customPlugins;
    vimrcConfig.vam.pluginDictionaries = [ { name = "coot"; } ];
  };
in [ vim ]
