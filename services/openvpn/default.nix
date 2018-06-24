{
  servers = {
    # systemctl {start|stop} openvpn-{attr}.service
    pia-ca = import ./pia/ca;
    pia-nl = import ./pia/nl;
    sumall = import ./sumall;
  };
}
