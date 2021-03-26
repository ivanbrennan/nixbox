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

    # Verify server certificate to protect against mitm attacks.
    remote-cert-tls server

    # The cipher the server is using.
    data-ciphers AES-256-CBC
    data-ciphers-fallback AES-256-CBC

    # Server has compression enabled, so we must also.
    compress lzo

    # Don't cache passwords in memory.
    auth-nocache

    ${builtins.readFile ./client.private.conf}
  '';
}
