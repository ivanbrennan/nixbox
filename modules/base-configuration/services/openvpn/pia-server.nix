{ remoteHost, remotePort, secrets }:

{
  autoStart = false;

  updateResolvConf = true;

  config = ''
    # This is a client config.
    client

    # Use a routed IP tunnel.
    dev tun

    proto udp

    # Don't bind to a specific local port number.
    nobind

    # Try to preserve some state across restarts.
    persist-key

    # Enable TLS
    tls-client

    # Verify server certificate to protect against mitm attacks.
    remote-cert-tls server

    # The cipher the server is using.
    data-ciphers AES-128-CBC
    data-ciphers-fallback AES-128-CBC

    # Server has compression enabled, so we must also.
    compress lzo

    # Don't cache passwords in memory
    auth-nocache

    # Disable channel renegotiation
    reneg-sec 0

    # Disable option inconsistency warnings
    # (in case server is running an old version)
    disable-occ

    # Server hostname and port.
    remote ${remoteHost} ${builtins.toString remotePort}

    # Authenticate via username/password.
    auth-user-pass ${secrets.openvpn_pia_login.path}

    # Certificate Authority
    ca ${secrets.openvpn_pia_ca.path}

    # Check server's cert against the revocation list.
    crl-verify ${secrets.openvpn_pia_crl.path}
  '';
}
