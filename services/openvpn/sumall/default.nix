{
  autoStart = false;

  updateResolvConf = true;

  config = ''
    # This is a client config.
    client

    # Use a routed IP tunnel.
    dev tun

    # Server is enforcing IPv4 only. We'll do the same.
    proto udp4

    # Don't bind to a specific local port number.
    nobind

    # Try to preserve some state across restarts.
    persist-key
    persist-tun

    # Verify server certificate to protect against mitm attacks.
    remote-cert-tls server

    # The cipher the server is using.
    cipher AES-256-CBC

    # Server has compression enabled, so we can too.
    compress lzo

    # Don't cache passwords in memory
    auth-nocache

    ${builtins.readFile ./client.private.conf}
  '';
}
