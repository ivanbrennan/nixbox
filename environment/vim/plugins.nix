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
      rev = "3c90419f686d0bbbbe036dd5abe7735d6ffc3cc8";
      sha256 = "02ggkpkwq6z5a2hcw5jwyxx2fr9pl7hwb7b3d8gahnw6awadl6px";
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
      rev = "1237fa3e33aa7693d54c265472f2ebfa750eeac8";
      sha256 = "0xkajx0ns04z6i2rldqjcgrb7jfbs9mr21wmlvxnb436n25cqyyq";
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
      rev = "23cc0a5f038cd049af000d650d3ab4921f920378";
      sha256 = "0xk74bq5y1xpjki2yrbll3flp76vwnqly7zb9fhnqlhpxm27wjf3";
    };
  };
  ftglue = pkgs.vimUtils.buildVimPlugin {
    name = "ftglue";
    src = pkgs.fetchFromGitHub {
      owner = "ivanbrennan";
      repo = "ftglue";
      rev = "a17718f984faf0c95dd9091531d6edfa44b00f7b";
      sha256 = "0mwyrgzkcmy5b9ylr7a8sp9fqsskcmi88p2q2hyfnq9jznk2prq2";
    };
  };
  hint = pkgs.vimUtils.buildVimPlugin {
    name = "hint";
    src = pkgs.fetchFromGitHub {
      owner = "ivanbrennan";
      repo = "hint";
      rev = "8f9430484918dca15d518a4390dfd7a462e715b9";
      sha256 = "0zs6g98fbmqris54va3h6xhm9b58gh7m71717i863hq0gv76gfdv";
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
      rev = "ad09e75e69b63708fa2ae810c7f30fd1b5c1f29d";
      sha256 = "0bi8f4x4mghpgsacj8hv3px40amzsii00rr52fyn981wrnw9xgib";
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
      rev = "e6daf39223912b87dafa71ffe8d650950d1485b1";
      sha256 = "0ly8gfvrkq9m1l55njg4c3w5d1jkdgmi9mdghy84hxnazsdwrzgp";
    };
  };
  refract = pkgs.vimUtils.buildVimPlugin {
    name = "refract";
    src = pkgs.fetchFromGitHub {
      owner = "ivanbrennan";
      repo = "refract";
      rev = "5b00cf6bcf943243b383e86166c6f47864ae3ed1";
      sha256 = "0pmwkxpk7w8g329qnqc5s305kgjsqwci55i8n2xqlbf4l8yamm8c";
    };
  };
  sohi = pkgs.vimUtils.buildVimPlugin {
    name = "sohi";
    src = pkgs.fetchFromGitHub {
      owner = "ivanbrennan";
      repo = "sohi";
      rev = "5a4b8e0cb75919085df8830aefe4cf83b6689dd8";
      sha256 = "1ps6rng7yhh47vb704ixc0sbiqj2sgx1xh4d5mjqk39p3gh91xm5";
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
  vim-grepper = pkgs.vimUtils.buildVimPlugin {
    name = "vim-grepper";
    src = pkgs.fetchFromGitHub {
      owner = "ivanbrennan";
      repo = "vim-grepper";
      rev = "7ac2533e891958a63d408cfbaf017462cb66018b";
      sha256 = "1vwl7b7ck7ph90yiv4b3qcgpln6qh9h4l9bwdjzlijrhh3cxc6vy";
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
      rev = "684cec57a8c4ce25408cdae5f7ff586979dc4a6c";
      sha256 = "1hrmqazd28k2wbl7c8mv833yfs8rq03wn8qksdddbisjdj9xrnjh";
    };
  };
  zoo = pkgs.vimUtils.buildVimPlugin {
    name = "zoo";
    src = pkgs.fetchFromGitHub {
      owner = "ivanbrennan";
      repo = "zoo";
      rev = "9af580fee42b144d7f4981737b1e704f2417f739";
      sha256 = "0hvm5hzz8y106kyxwyzj0341scgqk83k2hhmdbgb22zbmy9251fc";
    };
  };
}
