pkgs:

let
  customPlugins = {
    articulate = pkgs.vimUtils.buildVimPlugin {
      name = "articulate";
      src = pkgs.fetchFromGitHub {
        owner = "ivanbrennan";
        repo = "articulate";
        rev = "82d03316b67249a32cdddfc9a0385b1f4c2ff3a1";
        sha256 = "1cg4djcg3qh0hjic65ivkvcz1jcblahnvvi560qd9jvjm9j58kss";
      };
    };
    coot = pkgs.vimUtils.buildVimPlugin {
      name = "coot";
      src = pkgs.fetchFromGitHub {
        owner = "ivanbrennan";
        repo = "coot";
        rev = "dbba4c34a20e1cebcd54e62bec0de455f5615adb";
        sha256 = "14h0r5hkwx0r5rhzc56sdnkdczzy2nrv8xhyj2hsgwbyaa3a5j16";
      };
    };
    mline = pkgs.vimUtils.buildVimPlugin {
      name = "mline";
      src = pkgs.fetchFromGitHub {
        owner = "ivanbrennan";
        repo = "mline";
        rev = "7bffa6a6a1d5296dca2ea75170859307696068a7";
        sha256 = "0c6wgyvd4yvrr725z6ch90m9gx3hpqpyf1b9b5gcizd6r2bx6y5k";
      };
    };
    pinnacle = pkgs.vimUtils.buildVimPlugin {
      name = "pinnacle";
      src = pkgs.fetchFromGitHub {
        owner = "wincent";
        repo = "pinnacle";
        rev = "ec3373b33d289f2e7f2d8a324aef5e0b20f95b7b";
        sha256 = "1sv82861mf3z9rkxmlwaiwzbpa1ri6mhw67ngw67177xg4cghz6q";
      };
    };
    vim-matchit = pkgs.vimUtils.buildVimPlugin {
      name = "vim-matchit";
      src = pkgs.fetchFromGitHub {
        owner = "jwhitley";
        repo = "vim-matchit";
        rev = "57de3a754795fe325771bf0c3991905ae1d0246e";
        sha256 = "0k31j4fbzdilkl8bqi1lkljyamj298fb2d4shds84lr1bmz4mlqm";
      };
    };
    vim-unimpaired = pkgs.vimUtils.buildVimPlugin {
      name = "vim-unimpaired";
      src = pkgs.fetchFromGitHub {
        owner = "tpope";
        repo = "vim-unimpaired";
        rev = "c77939c4aff30b2ed68deb1752400ec15f17c3a2";
        sha256 = "0qd9as008r2vycls48bfb163rp7dddw7l495xn4l1gl00sh79cxy";
      };
    };
  };
  vim = pkgs.vim_configurable.customize {
    name = "vim";
    vimrcConfig.customRC = import ./vimrc;
    vimrcConfig.vam.knownPlugins = pkgs.vimPlugins // customPlugins;
    vimrcConfig.vam.pluginDictionaries =
      [ { name = "articulate"; }
        { name = "coot"; }
        { name = "fugitive"; }
        { name = "gundo"; }
        { name = "mline"; }
        { name = "pinnacle"; }
        { name = "surround"; }
        { name = "vim-matchit"; }
        { name = "vim-repeat"; }
        { name = "vim-unimpaired"; }
      ];
  };
in [ vim ]
