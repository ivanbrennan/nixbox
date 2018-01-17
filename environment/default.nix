{ pkgs, ... }:

{
  environment = {
    systemPackages = (import ./system-packages.nix) pkgs;
    variables = import ./variables.nix;
    etc = {
      "inputrc".source = pkgs.lib.mkForce ./etc/inputrc;
      "sysless".source = pkgs.sysless + "/etc/sysless";
    };
  };
}
