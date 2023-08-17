{ pkgs, ... }:

{
  enableCompletion = true;
  shellAliases = import ./aliases.nix pkgs;
  promptInit = builtins.readFile ./prompt.sh;
  interactiveShellInit = ''
    ${builtins.readFile ./bashrc}
    . ${pkgs.fzf}/share/fzf/key-bindings.bash
    . ${pkgs.etcdots}/share/etcdots/key-bindings.bash
    . ${pkgs.git}/share/bash-completion/completions/git-prompt.sh
  '';
}
