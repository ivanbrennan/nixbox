{ config }:

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

    # Server hostname and port.
    remote-random
    remote 'gw1.odeko.com' 1194
    remote 'gw2.odeko.com' 1194

    # If hostname resolve fails for remote, retry resolve for n seconds before failing.
    resolv-retry 60

    # Client needs same tls-auth key that the server uses.
    tls-auth ${config.sops.secrets.openvpn_odeko_ta.path} 1

    # SSL/TLS
    ca   ${config.sops.secrets.openvpn_odeko_ca.path}
    cert ${config.sops.secrets.openvpn_odeko_cert.path}
    key  ${config.sops.secrets.openvpn_odeko_key.path}

    # Currently the only way I've found to successfully start client via systemctl.
    # Otherwise systemd-ask-password prompts for the passphrase, but the prompt
    # is never shown. https://forums.openvpn.net/viewtopic.php?f=6&t=26408
    askpass ${config.sops.secrets.openvpn_odeko_askpass.path}
  '';
}
