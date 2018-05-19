{
  servers = {
    # systemctl {start|stop} openvpn-sumall.service
    sumall = import ./sumall;
  };
}
