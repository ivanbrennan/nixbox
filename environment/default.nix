{ pkgs, ... }:

let
  githubUsername = import ./github-username.private;
  githubToken = import ./github-token.private;
in {
  environment = {
    systemPackages = (import ./system-packages.nix) pkgs;
    variables = (import ./variables.nix) pkgs;
    etc = {
      "gitattributes".source = etc/gitattributes;
      "gitconfig".source = pkgs.lib.mkForce etc/gitconfig;
      "gittemplates/hooks/ctags" = {
        source = pkgs.lib.mkForce etc/gittemplates/hooks/ctags;
        mode = "0755";
      };
      "gittemplates/hooks/gtags" = {
        source = pkgs.lib.mkForce etc/gittemplates/hooks/gtags;
        mode = "0755";
      };
      "gittemplates/hooks/hasktags" = {
        source = pkgs.lib.mkForce etc/gittemplates/hooks/hasktags;
        mode = "0755";
      };
      "gittemplates/hooks/post-checkout" = {
        source = pkgs.lib.mkForce etc/gittemplates/hooks/post-checkout;
        mode = "0755";
      };
      "gittemplates/hooks/post-commit" = {
        source = pkgs.lib.mkForce etc/gittemplates/hooks/post-commit;
        mode = "0755";
      };
      "gittemplates/hooks/post-merge" = {
        source = pkgs.lib.mkForce etc/gittemplates/hooks/post-merge;
        mode = "0755";
      };
      "gittemplates/hooks/post-rewrite" = {
        source = pkgs.lib.mkForce etc/gittemplates/hooks/post-rewrite;
        mode = "0755";
      };
      "gittemplates/hooks/pre-push" = {
        source = pkgs.lib.mkForce etc/gittemplates/hooks/pre-push;
        mode = "0755";
      };
      "gitignore".source = etc/gitignore;
      "ripgreprc".source = pkgs.writeText "ripgreprc" ''
        --ignore-file=/etc/gitignore
      '';
      "inputrc".source = etc/inputrc;
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
      "xdg/kitty/kitty.conf".source = etc/xdg/kitty/kitty.conf;
      "zathurarc".source = etc/zathurarc;
    };
    extraInit = ''
      export XDG_CONFIG_DIRS="/etc/xdg''${XDG_CONFIG_DIRS:+:}$XDG_CONFIG_DIRS"
    '';
    # Needed for themes and backgrounds
    pathsToLink = [
      "/share" # TODO: https://github.com/NixOS/nixpkgs/issues/47173
    ];
  };
}
