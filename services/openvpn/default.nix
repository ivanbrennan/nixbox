config:

let
  pia-server = { remoteHost, remotePort }: (import ./pia-server.nix) {
    inherit config remoteHost remotePort;
  };
in
{
  servers = {
    # systemctl {start|stop} openvpn-{attr}.service
    pia-ca = pia-server {
      remoteHost = "ca-montreal.privateinternetaccess.com";
      remotePort = 1198;
    };
    pia-nl = pia-server {
      remoteHost = "nl-amsterdam.privacy.network";
      remotePort = 1198;
    };
    pia-swiss = pia-server {
      remoteHost = "swiss.privacy.network";
      remotePort = 1198;
    };
    odeko = import (./odeko-server.nix) { inherit config; };
  };
}
