{ config, lib, pkgs, ... }:

let

  vpns = builtins.map (x: "openvpn-${x}.service")
    (builtins.attrNames config.services.openvpn.servers);

  array = xs: "[${lib.concatMapStringsSep ", " (x: ''"${x}"'') xs}]";

in

{
  security = {
    sudo = {
      extraRules = lib.mkAfter [
        {
          users = [ "ivan" ];
          commands = [
            {
              command = "${pkgs.resound}/bin/remod-sof";
              options = [ "NOPASSWD" "SETENV" ];
            }
          ];
        }
      ];

      # The NixOS sudo module already preserves SSH_AUTH_SOCK, but let's also
      # preserve ssh client connection environment variables.
      extraConfig = ''
        Defaults env_keep+="SSH_CLIENT SSH_CONNECTION SSH_TTY"
      '';
    };

    polkit = {
      enable = true;
      extraConfig = ''
        polkit.addRule(function(action, subject) {
          const id = action.id;
          if (id == "org.freedesktop.systemd1.manage-units" ||
              id == "org.freedesktop.systemd1.manage-unit-files") {
            if (${array vpns}.indexOf(action.lookup("unit")) !== -1) {
              const verb = action.lookup("verb");
              if (verb == "start" ||
                  verb == "stop" ||
                  verb == "restart") {
                if (subject.isInGroup("wheel")) {
                  return polkit.Result.YES;
                }
              }
            }
          }
        });
      '';
    };
  };
}
