{
  ffi = {
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0zw6pbyvmj8wafdc7l5h7w20zkp1vbr2805ql5d941g2b20pk4zr";
      type = "gem";
    };
    version = "1.9.23";
  };
  interactive_editor = {
    dependencies = ["spoon"];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0g7mll7k1ccam22z4hcdi7j5nc86ls3i8s1dhvwkmwz75nj934bm";
      type = "gem";
    };
    version = "0.0.11";
  };
  spoon = {
    dependencies = ["ffi"];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1x5kp9dzq9lvsg81xjlpagsiw3qnnsb2wnq8qdwp6vykyql8xl9s";
      type = "gem";
    };
    version = "0.0.6";
  };
}
