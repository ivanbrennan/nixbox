{ config, lib, ... }:

let

  vpns = builtins.map (x: "openvpn-${x}.service")
    (builtins.attrNames config.services.openvpn.servers);

  array = xs: "[${lib.concatMapStringsSep ", " (x: ''"${x}"'') xs}]";

in

{
  security = {
    polkit = {
      enable = true;
      extraConfig = ''
        polkit.addRule(function(action, subject) {
          let id = action.id;
          if (id == "org.freedesktop.systemd1.manage-units" ||
              id == "org.freedesktop.systemd1.manage-unit-files") {
            if (${array vpns}.includes(action.lookup("unit"))) {
              let verb = action.lookup("verb");
              if (verb == "start" ||
                  verb == "stop" ||
                  verb == "restart") {
                if (subject.isInGroup("wheel")) {
                  return polkit.Result.YES
                }
              }
            }
          }
        });
      '';
    };
  };
}
