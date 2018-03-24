pkgs:

let
  publicPlugins = with pkgs.vimPlugins;
    [ commentary
      fugitive
      gundo
      surround
      vim-easy-align
      vim-eunuch
      vim-nix
      vim-repeat
      vim-ruby
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
    coherent = pkgs.vimUtils.buildVimPlugin {
      name = "coherent";
      src = pkgs.fetchFromGitHub {
        owner = "ivanbrennan";
        repo = "coherent";
        rev = "fe9e1f5e05b407902a668e0c484cdb45361809d5";
        sha256 = "058mysq7zsykqw2gxmnj6rzfblqn2xqjy0zgxyj0frfks0j6qizi";
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
    dirvish = pkgs.vimUtils.buildVimPlugin {
      name = "dirvish";
      src = pkgs.fetchFromGitHub {
        owner = "justinmk";
        repo = "vim-dirvish";
        rev = "e535cca83c8eb0e4d1a2c0861c3a03b7f6d67705";
        sha256 = "0f08w5cipad5q3wa7klw5w2ld6abkr1iy467zsqz6f65hbhp5hnd";
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
    ftglue = pkgs.vimUtils.buildVimPlugin {
      name = "ftglue";
      src = pkgs.fetchFromGitHub {
        owner = "ivanbrennan";
        repo = "ftglue";
        rev = "23bb1082183641dee93ad81d1af4b6f951b2c495";
        sha256 = "1pklr7hc0rky0wax1absjrji0avy7ky557km3xv8a7ljaf9vdfgj";
      };
    };
    iota = pkgs.vimUtils.buildVimPlugin {
      name = "iota";
      src = pkgs.fetchFromGitHub {
        owner = "ivanbrennan";
        repo = "iota";
        rev = "e1317dbd2355a3694c072350150ff0f9982f98b5";
        sha256 = "1qznd5rw6a4ad6s5gwqimkib51wbgwancwykb37pqilg4qd6i5jb";
      };
    };
    listical = pkgs.vimUtils.buildVimPlugin {
      name = "listical";
      src = pkgs.fetchFromGitHub {
        owner = "ivanbrennan";
        repo = "listical";
        rev = "b66979f3d530d970e43f190e7d67e8ab03044544";
        sha256 = "1byiz75k4xfhcf2qp45lvx0dslpxzxn03f0h57dn8zikm0bx35mm";
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
        rev = "3ef4d2c8a4987ec103935d32154b656a76130a4d";
        sha256 = "0475x9ww2k8sg44gp7839r8dniyjs62d27r4wfibk8768yl84vpi";
      };
    };
    ocursor = pkgs.vimUtils.buildVimPlugin {
      name = "ocursor";
      src = pkgs.fetchFromGitHub {
        owner = "ivanbrennan";
        repo = "ocursor";
        rev = "6980b0f611ad7f02280800855c6098319a3d906e";
        sha256 = "08bzbjjhj0qg9rkgs62frpj94sdgvyxzxclx6y02gq7v18qba743";
      };
    };
    optcycle = pkgs.vimUtils.buildVimPlugin {
      name = "optcycle";
      src = pkgs.fetchFromGitHub {
        owner = "ivanbrennan";
        repo = "optcycle";
        rev = "29b14a7a4320bf12bb35bcee565e9d0a7d32bfe0";
        sha256 = "11887nfzy63h80qxrj4fwn069jar19z86z2b3bky4hkjl60myr6k";
      };
    };
    sohi = pkgs.vimUtils.buildVimPlugin {
      name = "sohi";
      src = pkgs.fetchFromGitHub {
        owner = "ivanbrennan";
        repo = "sohi";
        rev = "837bb832e71878edf1a4398742b8e674f57e66a5";
        sha256 = "04lq8x6525hnaqxiy76zizqwpkalgcfpj5kg3jrjjzplycf238c4";
      };
    };
    super-shell-indent = pkgs.vimUtils.buildVimPlugin {
      name = "Super-Shell-Indent";
      src = pkgs.fetchFromGitHub {
        owner = "vim-scripts";
        repo = "Super-Shell-Indent";
        rev = "eee1c2ef40f333049c45c6cadd1a2b9fa58c8488";
        sha256 = "1k7mr8q7jbhqhg07a1m00ihcrvsnmg49rp8y7sdna20dd5jd3yfd";
      };
    };
    tabtab = pkgs.vimUtils.buildVimPlugin {
      name = "tabtab";
      src = pkgs.fetchFromGitHub {
        owner = "ivanbrennan";
        repo = "tabtab";
        rev = "1ef74cd0f40e498f00f9d00dc985231c6b14d171";
        sha256 = "1rzr4gazvwxv3y8wk6gdv58a16zc41yllnsmhpyhm19xjmijxsfy";
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
    vim-projectionist = pkgs.vimUtils.buildVimPlugin {
      name = "vim-projectionist";
      src = pkgs.fetchFromGitHub {
        owner = "tpope";
        repo = "vim-projectionist";
        rev = "d20f2a25fe820c5d0abf4b584c46203ecf067f2d";
        sha256 = "1vn55f3jls06bsavk4vf9fy9hq0izbg57b69f6j58kv887xvynlv";
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
        rev = "bbcfcff6b6d31269af436a5f66645805949b0161";
        sha256 = "0y4wwcxw9i2spykdrj8inlgy9pqdqni2n6gn7dph21imfc4zlhz4";
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

    vimrcConfig.customRC = builtins.readFile "${pkgs.dotvim}/vimrc";
  };
in [ vim ]
