{ pkgs, ... }:

{
  environment = {
    systemPackages = (import ./system-packages.nix) pkgs;
    variables = import ./variables.nix;
    etc = {
      "gitattributes".source = "${pkgs.etconfig}/etc/gitattributes";
      "gitconfig".source = pkgs.lib.mkForce ./etc/gitconfig;
      "gitignore".source = "${pkgs.etconfig}/etc/gitignore";
      "inputrc".source = "${pkgs.etconfig}/etc/inputrc";
      "sysless".source = "${pkgs.sysless}/etc/sysless";
    };
  };
}
