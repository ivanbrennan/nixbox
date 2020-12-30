import ../server.nix {
  name = "pia-nl";
  remote = builtins.readFile ./remote.private;
}
