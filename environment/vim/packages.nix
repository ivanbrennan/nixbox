pkgs:

let
  publicPlugins = with pkgs.vimPlugins;
    [ fugitive
      gundo
      surround
      vim-nix
      vim-repeat
    ];

  privatePlugins = map (key: builtins.getAttr key privateSet)
                       (builtins.attrNames privateSet);

  privateSet = {
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
        rev = "7930805b43ec89b2ef43991e15f2df8c9b2d64ff";
        sha256 = "0f8qlqi67jb6mm5x9iww9ihwya8x0ansgm1xka9siccwb68078ck";
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
    edot = pkgs.vimUtils.buildVimPlugin {
      name = "edot";
      src = pkgs.fetchFromGitHub {
        owner = "ivanbrennan";
        repo = "edot";
        rev = "0e969d9a9850e1ed28edce1c4be1eb37543f7b84";
        sha256 = "0c6i8m75q2n6s4ik90rvg2hyzygmhnpv8kjvwk7awxjj5w0bgf3v";
      };
    };
  };

  vim = pkgs.vim_configurable.customize {
    name = "vim";

    vimrcConfig.packages.myPackage = {
      # loaded on launch
      start = publicPlugins ++ privatePlugins;
      # manually loadable by calling `:packadd $plugin-name`
      opt = [ ];
      # To automatically load a plugin when opening a filetype, add vimrc lines like:
      # autocmd FileType php :packadd phpCompletion
    };

    vimrcConfig.customRC = import ./vimrc;
  };
in [ vim ]
