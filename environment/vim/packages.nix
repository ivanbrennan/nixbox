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
        rev = "f49d13b87728c1e3732a7add55a789c93fa5f849";
        sha256 = "0s82lijwabk0lm64nidx6yv536l7ig04pvyn9333ncwxpa6zyfky";
      };
    };
    coot = pkgs.vimUtils.buildVimPlugin {
      name = "coot";
      src = pkgs.fetchFromGitHub {
        owner = "ivanbrennan";
        repo = "coot";
        rev = "82152e1eb891b06c360102a44fcc1d895e150eb7";
        sha256 = "0pcjfbqa7y60qi49j8pn3ym2pj6d9kikda3c9347sa9rw4ri7b81";
      };
    };
    ftglue = pkgs.vimUtils.buildVimPlugin {
      name = "ftglue";
      src = pkgs.fetchFromGitHub {
        owner = "ivanbrennan";
        repo = "ftglue";
        rev = "ad5ef42075c5959f669412b9218fa7f100cf3890";
        sha256 = "1py6vswillkm0b57j4wxjrbgwd4wbggl69fpmy66jd14412cm2r7";
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
        { name = "pinnacle"; }
        { name = "surround"; }
        { name = "vim-matchit"; }
        { name = "vim-repeat"; }
        { name = "vim-unimpaired"; }
        { name = "vmacs"; }
      ];
  };
in [ vim ]
