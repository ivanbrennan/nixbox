{ pkgs, ... }:

let
  githubUsername = import ./github-username.private;
  githubToken = import ./github-token.private;
in {
  environment = {
    systemPackages = (import ./system-packages.nix) pkgs;
    variables = (import ./variables.nix) pkgs;
    etc = {
      "gitattributes".source = "${pkgs.etcdots}/etc/gitattributes";
      "gitconfig".source = pkgs.lib.mkForce etc/gitconfig;
      "gitignore".source = "${pkgs.etcdots}/etc/gitignore";
      "inputrc".source = "${pkgs.etcdots}/etc/inputrc";
      "nix/netrc".source = pkgs.writeText "netrc" ''
        machine github.com
        login ${githubUsername}
        password ${githubToken}

        machine api.github.com
        login ${githubUsername}
        password ${githubToken}
      '';
      "xdg/gtk-3.0/settings.ini".source = etc/xdg/gtk-3.0/settings.ini;
      "xdg/gtk-2.0/gtkrc".source = etc/xdg/gtk-2.0/gtkrc;
      "xdg/termite/config".source = etc/xdg/termite/config;
    };
    extraInit = ''
      export XDG_CONFIG_DIRS="/etc/xdg''${XDG_CONFIG_DIRS:+:}$XDG_CONFIG_DIRS"
    '';
  };
}
