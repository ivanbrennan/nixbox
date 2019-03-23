{
  ffi = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0j8pzj8raxbir5w5k6s7a042sb5k02pg0f8s4na1r5lan901j00p";
      type = "gem";
    };
    version = "1.10.0";
  };
  interactive_editor = {
    dependencies = ["spoon"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0g7mll7k1ccam22z4hcdi7j5nc86ls3i8s1dhvwkmwz75nj934bm";
      type = "gem";
    };
    version = "0.0.11";
  };
  spoon = {
    dependencies = ["ffi"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1x5kp9dzq9lvsg81xjlpagsiw3qnnsb2wnq8qdwp6vykyql8xl9s";
      type = "gem";
    };
    version = "0.0.6";
  };
}
