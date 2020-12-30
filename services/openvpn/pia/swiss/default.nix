import ../server.nix {
  name = "pia-swiss";
  remote = builtins.readFile ./remote.private;
}
