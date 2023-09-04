{ pkgs, ... }:

{
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
      "xdg/nvim/after/ftplugin/qf.lua".source = etc/xdg/nvim/after/ftplugin/qf.lua;
      "xdg/nvim/after/plugin/keymap.lua".source = etc/xdg/nvim/after/plugin/keymap.lua;
      "xdg/nvim/after/syntax/c.vim".source = etc/xdg/nvim/after/syntax/c.vim;
      "xdg/nvim/after/syntax/gitcommit.lua".source = etc/xdg/nvim/after/syntax/gitcommit.lua;
      "xdg/nvim/after/syntax/readline.vim".source = etc/xdg/nvim/after/syntax/readline.vim;
      "xdg/nvim/after/syntax/sh.vim".source = etc/xdg/nvim/after/syntax/sh.vim;
      "xdg/gtk-3.0/settings.ini".source = etc/xdg/gtk-3.0/settings.ini;
      "xdg/gtk-2.0/gtkrc".source = etc/xdg/gtk-2.0/gtkrc;
      "xdg/kitty/kitty.conf".source = etc/xdg/kitty/kitty.conf;
      "zathurarc".source = etc/zathurarc;
      "zellij/config.kdl".source = etc/zellij/config.kdl;
      "zellij/layouts/default.kdl".source = etc/zellij/layouts/default.kdl;
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
