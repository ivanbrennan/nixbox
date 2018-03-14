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
    bstack = pkgs.vimUtils.buildVimPlugin {
      name = "bstack";
      src = pkgs.fetchFromGitHub {
        owner = "ivanbrennan";
        repo = "bstack";
        rev = "3c90419f686d0bbbbe036dd5abe7735d6ffc3cc8";
        sha256 = "02ggkpkwq6z5a2hcw5jwyxx2fr9pl7hwb7b3d8gahnw6awadl6px";
      };
    };
    coot = pkgs.vimUtils.buildVimPlugin {
      name = "coot";
      src = pkgs.fetchFromGitHub {
        owner = "ivanbrennan";
        repo = "coot";
        rev = "bc8ffbc036cc0d1f8d2fe20f1fc4f10e1e637931";
        sha256 = "0g0551ldkjl1cz5grlm1agbp1q7daxdygpr2qaa1swjb9c7wgfg0";
      };
    };
    ftglue = pkgs.vimUtils.buildVimPlugin {
      name = "ftglue";
      src = pkgs.fetchFromGitHub {
        owner = "ivanbrennan";
        repo = "ftglue";
        rev = "3d925e0f314db7c3855176a5fa502fc1d72e4368";
        sha256 = "0m5m9y50r7d6yb3afi8188clf3vipbv3cpbp33mrrkpbgxcbhip3";
      };
    };
    loupe = pkgs.vimUtils.buildVimPlugin {
      name = "loupe";
      src = pkgs.fetchFromGitHub {
        owner = "ivanbrennan";
        repo = "loupe";
        rev = "188509d4dc8089e1690fc62d2d6df86a60b4c9fe";
        sha256 = "1il8c873661bwl8d9lil9zkq5pfys5z1cnh2v0jpw2831zw0cjq4";
      };
    };
    mline = pkgs.vimUtils.buildVimPlugin {
      name = "mline";
      src = pkgs.fetchFromGitHub {
        owner = "ivanbrennan";
        repo = "mline";
        rev = "b817df9f46508b4e213753bd7a6f8830d677eb0d";
        sha256 = "1rm47g0hfjq2x6g38hc2lqb8cgcml4is8gxhqdmdidmr2msxs0lp";
      };
    };
    optcycle = pkgs.vimUtils.buildVimPlugin {
      name = "optcycle";
      src = pkgs.fetchFromGitHub {
        owner = "ivanbrennan";
        repo = "optcycle";
        rev = "ab3baa018218d347b6b26589a4774c9b25e31021";
        sha256 = "1y4qn6icdgwi41dnbf9h938j7598i8217aqlg916lxv0vffa6ajr";
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
    vmacs = pkgs.vimUtils.buildVimPlugin {
      name = "vmacs";
      src = pkgs.fetchFromGitHub {
        owner = "ivanbrennan";
        repo = "vmacs";
        rev = "30c5972f1b9cf53162ca1562d83643aae9d31c66";
        sha256 = "10q2bq4vn63z2pdzxfvm33pdmw89k77wyai9nqff30nsz895b40s";
      };
    };
  };
  vim = pkgs.vim_configurable.customize {
    name = "vim";
    vimrcConfig.customRC = import ./vimrc;
    vimrcConfig.vam.knownPlugins = pkgs.vimPlugins // customPlugins;
    vimrcConfig.vam.pluginDictionaries =
      [ { name = "articulate"; }
        { name = "bstack"; }
        { name = "coot"; }
        { name = "ftglue"; }
        { name = "fugitive"; }
        { name = "gundo"; }
        { name = "loupe"; }
        { name = "mline"; }
        { name = "optcycle"; }
        { name = "pinnacle"; }
        { name = "surround"; }
        { name = "vim-matchit"; }
        { name = "vim-nix"; }
        { name = "vim-repeat"; }
        { name = "vim-unimpaired"; }
        { name = "vmacs"; }
      ];
  };
in [ vim ]
