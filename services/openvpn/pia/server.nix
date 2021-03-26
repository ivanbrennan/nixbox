{ remote
, private ? builtins.readFile ./client.private.conf
}:

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
    ${remote}

    ${private}
  '';
}
