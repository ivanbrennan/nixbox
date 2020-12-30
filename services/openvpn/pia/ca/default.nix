import ../server.nix {
  name = "pia-ca";
  remote = builtins.readFile ./remote.private;
}
