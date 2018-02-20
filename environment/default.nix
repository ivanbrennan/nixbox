{ pkgs, ... }:

{
  environment = {
    systemPackages = (import ./system-packages.nix) pkgs;
    variables = import ./variables.nix;
    etc = {
      "gitattributes".source = pkgs.lib.mkForce ./etc/gitattributes;
      "gitconfig".source = pkgs.lib.mkForce ./etc/gitconfig;
      "gitignore".source = pkgs.lib.mkForce ./etc/gitignore;
      "inputrc".source = pkgs.lib.mkForce ./etc/inputrc;
      "sysless".source = pkgs.sysless + "/etc/sysless";
    };
  };
}
