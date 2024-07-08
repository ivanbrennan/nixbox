{ pkgs, ... }:

{
  completion.enable = true;
  shellAliases = import ./aliases.nix pkgs;
  promptInit = builtins.readFile ./prompt.sh;
  interactiveShellInit = ''
    ${builtins.readFile ./bashrc}
    . ${pkgs.fzf}/share/fzf/key-bindings.bash
    . ${pkgs.etcdots}/share/etcdots/key-bindings.bash
    . ${pkgs.git}/share/bash-completion/completions/git-prompt.sh

    # https://github.com/tadfisher/pass-otp/issues/137#issuecomment-1942254432
    . ${pkgs.pass.extensions.pass-otp}/share/bash-completion/completions/pass-otp
  '';
}
