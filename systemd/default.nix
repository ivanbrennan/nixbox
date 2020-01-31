{ pkgs, ... }:

{
  systemd.user.services.xcape = {
    enable = true;
    description = "xcape";
    after = [ "graphical-session-pre.target" ];
    partOf = [ "graphical-session.target" ];
    wantedBy = [ "graphical-session.target" ];
    serviceConfig = {
      Type = "forking";
      ExecStart = "${pkgs.xcape}/bin/xcape";
    };
  };
}
