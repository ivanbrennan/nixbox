pkgs:

{
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
      rev = "99e43e01cdfdd8758db5e0eb7f94511b41adb4f3";
      sha256 = "1rlvqxxka2big3amq1fjd3ic9p01pqhpxvy03s40hr6f79r3phbp";
    };
  };

  coherent = pkgs.vimUtils.buildVimPlugin {
    name = "coherent";
    src = pkgs.fetchFromGitHub {
      owner = "ivanbrennan";
      repo = "coherent";
      rev = "6ad6883536d68322725ac01320aea11dc231f686";
      sha256 = "1f79hiiippsq0349acd1zblcl0c1i8p1id5p2qfmz0cx0fgrvci4";
    };
  };

  coot = pkgs.vimUtils.buildVimPlugin {
    name = "coot";
    src = pkgs.fetchFromGitHub {
      owner = "ivanbrennan";
      repo = "coot";
      rev = "4c62e0174fb68241dae3f64b54c023f9aceff59b";
      sha256 = "1biyvmgcgvlzfzw26gcbfil6b8hw4n55678y9hzdaz1p3pcng594";
    };
  };

  dirvish = pkgs.vimUtils.buildVimPlugin {
    name = "dirvish";
    src = pkgs.fetchFromGitHub {
      owner = "justinmk";
      repo = "vim-dirvish";
      rev = "0ad27e1e9161057ad83c6a96e9a4b8acd6e17f98";
      sha256 = "19a21sr7c5lk8b6vx3gsyla03c32lixgk670md4n79gslxy40f6a";
    };
  };

  edot = pkgs.vimUtils.buildVimPlugin {
    name = "edot";
    src = pkgs.fetchFromGitHub {
      owner = "ivanbrennan";
      repo = "edot";
      rev = "c8d24897cd46e9aaf2ed2b1f53dceabeb4be6dcf";
      sha256 = "0jmpjg7kws601mb3wvmjj0j7syvfdqx0gcjyss0h89q0jmqzh0ri";
    };
  };

  ftglue = pkgs.vimUtils.buildVimPlugin {
    name = "ftglue";
    src = pkgs.fetchFromGitHub {
      owner = "ivanbrennan";
      repo = "ftglue";
      rev = "b57df17715633d3ebb0f76fc77334051d02746da";
      sha256 = "03j6ld0vhlffvdcbm41c7rx8vl15va1mi9ryb4v3inrbnk942bnr";
    };
  };

  hint = pkgs.vimUtils.buildVimPlugin {
    name = "hint";
    src = pkgs.fetchFromGitHub {
      owner = "ivanbrennan";
      repo = "hint";
      rev = "abde012bdb4d71ba5c56519afb7798fc86ad4f95";
      sha256 = "0jx0b8md10cjfl1dva6zy537ampv6alidzbh6cjs4lg9m9y7g76h";
    };
  };

  iota = pkgs.vimUtils.buildVimPlugin {
    name = "iota";
    src = pkgs.fetchFromGitHub {
      owner = "ivanbrennan";
      repo = "iota";
      rev = "eb6409db09f7b547f769dfd0a00de4396e7af14c";
      sha256 = "159hfipy63vlykxcqwb9wzp6mfzv5v7ibj5lznjpzmkfn36sb867";
    };
  };

  listical = pkgs.vimUtils.buildVimPlugin {
    name = "listical";
    src = pkgs.fetchFromGitHub {
      owner = "ivanbrennan";
      repo = "listical";
      rev = "b9fb9b05c268d8c5986e8caf2af8aa3deab0a168";
      sha256 = "0bvbi05zln9h3wk0396g2gd53a3vqy2k8cyxz3957gkg7xn8h1w5";
    };
  };

  mline = pkgs.vimUtils.buildVimPlugin {
    name = "mline";
    src = pkgs.fetchFromGitHub {
      owner = "ivanbrennan";
      repo = "mline";
      rev = "69195fb88fa406f51a0ada9a845815d5863dd2b4";
      sha256 = "0pjp75f3av5p3vjrb64r0cib3g2pxqlk1s54cr8pl03qv3yp53w7";
    };
  };

  ocursor = pkgs.vimUtils.buildVimPlugin {
    name = "ocursor";
    src = pkgs.fetchFromGitHub {
      owner = "ivanbrennan";
      repo = "ocursor";
      rev = "c21602a41d40b76117af029cb98b4e4e459bf6cd";
      sha256 = "0c8ka85821b24g5l37lp2kigr44p6l567jdaa2smycqx8yspwamm";
    };
  };

  optcycle = pkgs.vimUtils.buildVimPlugin {
    name = "optcycle";
    src = pkgs.fetchFromGitHub {
      owner = "ivanbrennan";
      repo = "optcycle";
      rev = "9848e68c3de086f82cf66613ee141ff0a5aba46e";
      sha256 = "0y35j171lygfd4nhf2iqf0zapam24fzkdy7h6ks967zsvsl2dxmn";
    };
  };

  refract = pkgs.vimUtils.buildVimPlugin {
    name = "refract";
    src = pkgs.fetchFromGitHub {
      owner = "ivanbrennan";
      repo = "refract";
      rev = "ef9cc91cbb571e54e32ccaa556e56491ba8c4385";
      sha256 = "1ycb6gjd202smx7q8yvx7576v86sz2l6xkg8ps0ry0minirgaf6g";
    };
  };

  sohi = pkgs.vimUtils.buildVimPlugin {
    name = "sohi";
    src = pkgs.fetchFromGitHub {
      owner = "ivanbrennan";
      repo = "sohi";
      rev = "89c004a3ecd197aed2bf8f63169ccfd29ff8b40e";
      sha256 = "02yh4spwifgbnm8zmnnkvbl3qpxkksphax3nl8gsx6l8w3k0y9pq";
    };
  };

  splitjoin = pkgs.vimUtils.buildVimPlugin {
    name = "splitjoin";
    src = pkgs.fetchFromGitHub {
      owner = "AndrewRadev";
      repo = "splitjoin.vim";
      rev = "98d860151e4ea9b7845bfdd7e173b660d18fe2b0";
      sha256 = "1czw3hrhf8pw9bq3mfjbald2yqy81jjdnvn05qfq1b80navxff1q";
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
      rev = "cfd2119fdfd22c62af63a11417d6a2bb09b12534";
      sha256 = "1f5f4p3y31ricyjsfgpizxfrqf6wa4iybc0vlh14rfix82rkrzzj";
    };
  };

  vim-grepper = pkgs.vimUtils.buildVimPlugin {
    name = "vim-grepper";
    src = pkgs.fetchFromGitHub {
      owner = "ivanbrennan";
      repo = "vim-grepper";
      rev = "7ac2533e891958a63d408cfbaf017462cb66018b";
      sha256 = "1vwl7b7ck7ph90yiv4b3qcgpln6qh9h4l9bwdjzlijrhh3cxc6vy";
    };
  };

  vim-javascript-syntax = pkgs.vimUtils.buildVimPlugin {
    name = "vim-javascript-syntax";
    src = pkgs.fetchFromGitHub {
      owner = "jelera";
      repo = "vim-javascript-syntax";
      rev = "139ec9080f219536a94281aef7980654ab7c1a1c";
      sha256 = "18468dljr9fqfy89jfs8ahcfj6a26cp5c4iqi526wwj25irbxf71";
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

  vim-racket = pkgs.vimUtils.buildVimPlugin {
    name = "vim-racket";
    src = pkgs.fetchFromGitHub {
      owner = "wlangstroth";
      repo = "vim-racket";
      rev = "f76fde9afbc008b7468c9ea026cbe3840af5f5ef";
      sha256 = "1cs6kyw9565mdpyifvnp6lw9n0i31ahfqn48pg1n5h49bvn9667x";
    };
  };

  vim-rails = pkgs.vimUtils.buildVimPlugin {
    name = "vim-rails";
    src = pkgs.fetchFromGitHub {
      owner = "tpope";
      repo = "vim-rails";
      rev = "39cb87dbbac742dad68908c1c645b8f202d8f943";
      sha256 = "0n9h56gicy4cdisqmd9rd3p2z5syadd0iv0xkv04glwd206k7naf";
    };
  };

  vim-rake = pkgs.vimUtils.buildVimPlugin {
    name = "vim-rake";
    src = pkgs.fetchFromGitHub {
      owner = "tpope";
      repo = "vim-rake";
      rev = "79e51f17d26e2f31321af94ffd45bc9060623fdb";
      sha256 = "0k5n06fwqlmkkmf8a7n9whrhc57l54fwl5dnr2hc1lyfjn8bqv45";
    };
  };

  vim-spec-runner = pkgs.vimUtils.buildVimPlugin {
    name = "vim-spec-runner";
    src = pkgs.fetchFromGitHub {
      owner = "gabebw";
      repo = "vim-spec-runner";
      rev = "08b31f2963073863de59df4aec1b22c610745968";
      sha256 = "19npg6q6ff5x9i6xaqj4pxzx3jg1nmbchj6vr8srn4af9znmzh6r";
    };
  };

  vim-tmux-runner = pkgs.vimUtils.buildVimPlugin {
    name = "vim-tmux-runner";
    src = pkgs.fetchFromGitHub {
      owner = "christoomey";
      repo = "vim-tmux-runner";
      rev = "30ed8912f5051eeffee70a478e8db63938a448f8";
      sha256 = "1svizngc4p1cnyqw7qbsxj215ls2nw0c2i041cdmlgdma1yyns4v";
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
      rev = "668f0a9273d5368fe8d8ac30662f6f5dacc47574";
      sha256 = "1yvhj2csskfaf1yqslc3hiv9y0i4pp26m20pakrw2k3dal8f4agd";
    };
  };

  vmux = pkgs.vimUtils.buildVimPlugin {
    name = "vmux";
    src = pkgs.fetchFromGitHub {
      owner = "ivanbrennan";
      repo = "vmux";
      rev = "2773a8772f097f5ae491dc33ad2231e5417066db";
      sha256 = "18g6zhfcp30lxcdgqr45fpdw0aqvi5zvm9m1ag0abc84c2lk7h3g";
    };
  };

  wmgraphviz = pkgs.vimUtils.buildVimPlugin {
    name = "wmgraphviz";
    src = pkgs.fetchFromGitHub {
      owner = "wannesm";
      repo = "wmgraphviz.vim";
      rev = "eff46932ef8324ab605c18619e94f6b631d805e2";
      sha256 = "1l3qh6293v5rj2khr90i0pgybfvvifvclhla4d1pzdvi40csi0xs";
    };
  };

  zoo = pkgs.vimUtils.buildVimPlugin {
    name = "zoo";
    src = pkgs.fetchFromGitHub {
      owner = "ivanbrennan";
      repo = "zoo";
      rev = "7f5f8575bfb0daab2b40e6472bb674047d6b4713";
      sha256 = "05aqj6mcccnb35ddy4k721ydfqzp28lgnv8sj4rh5n1pnc9q2pbn";
    };
  };
}
