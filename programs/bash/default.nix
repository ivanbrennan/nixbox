{
  enableCompletion = true;
  shellAliases = import ./aliases.nix;
  interactiveShellInit = builtins.readFile ./bashrc;
  promptInit = builtins.readFile ./prompt.sh;
}
