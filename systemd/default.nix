{ pkgs, ... }:

{
  systemd.user.services."xcape" = {
    enable = true;
    description = "Xcape: Ctrl is Esc when pressed alone";
    wantedBy = [ "default.target" ];
    serviceConfig.Type = "forking";
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStart = "${pkgs.xcape}/bin/xcape";
  };
}
