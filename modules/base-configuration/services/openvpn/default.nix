{ secrets }:

let
  pia-server = import ./pia-server.nix;
  odeko-server = import ./odeko-server.nix;
in
{
  servers = {
    # systemctl {start|stop} openvpn-{attr}.service
    pia-ca = pia-server {
      remoteHost = "ca-montreal.privateinternetaccess.com";
      remotePort = 1198;
      inherit secrets;
    };
    pia-nl = pia-server {
      remoteHost = "nl-amsterdam.privacy.network";
      remotePort = 1198;
      inherit secrets;
    };
    pia-swiss = pia-server {
      remoteHost = "swiss.privacy.network";
      remotePort = 1198;
      inherit secrets;
    };
    odeko = odeko-server { inherit secrets; };
  };
}
