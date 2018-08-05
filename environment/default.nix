{ pkgs, ... }:

{
  environment = {
    systemPackages = (import ./system-packages.nix) pkgs;
    variables = (import ./variables.nix) pkgs;
    etc = {
      "gitattributes".source = "${pkgs.etcdots}/etc/gitattributes";
      "gitconfig".source = pkgs.lib.mkForce ./etc/gitconfig;
      "gitignore".source = "${pkgs.etcdots}/etc/gitignore";
      "inputrc".source = "${pkgs.etcdots}/etc/inputrc";
      "nix/netrc".source = pkgs.writeText "netrc" ''
        machine github.com
        login ${import ./github-username.private}
        password ${import ./github-token.private}
      '';
    };
  };
}
