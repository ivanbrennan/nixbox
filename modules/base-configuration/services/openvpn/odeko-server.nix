{ secrets }:

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

    ###

    # Both gateways listed; remote-random distributes load across the pair.
    remote gw1.odeko.com 1194
    remote gw2.odeko.com 1194
    remote-random

    # resolv-retry handles transient DNS failures during failover.
    resolv-retry infinite

    # Notify the server on client exit so the session slot is freed within
    # ~5s (CC-EEN delayed-exit) instead of ~4min (ping-restart timeout).
    # The "2" sends two notify packets to tolerate UDP loss.
    explicit-exit-notify 2

    # Don't bind to a specific local port number.
    nobind

    # Try to preserve some state across restarts.
    persist-key
    persist-tun

    # Require server cert presents EKU=serverAuth and CN starting with "gw"
    # (server certs are issued for CN=gw1.odeko.com / CN=gw2.odeko.com
    # regardless of the DNS name the client uses to connect).
    remote-cert-tls server
    verify-x509-name gw name-prefix

    # TLS 1.3 only.
    tls-version-min 1.3

    # AEAD-only data ciphers; "auth" is ignored by AEAD modes but kept for
    # compat with older clients that haven't fully migrated to data-ciphers.
    data-ciphers AES-256-GCM:CHACHA20-POLY1305:AES-128-GCM
    data-ciphers-fallback AES-256-GCM
    cipher AES-256-GCM
    auth SHA256

    # Don't cache the user's key passphrase between sessions.
    auth-nocache

    # Recommended verbosity level. Logs connection sequences, routing changes,
    # warnings, and non-fatal errors.
    verb 3

    # SSL/TLS
    ca   ${secrets.openvpn_odeko_ca.path}
    cert ${secrets.openvpn_odeko_cert.path}
    key  ${secrets.openvpn_odeko_key.path}
    tls-crypt-v2 ${secrets.openvpn_odeko_tls_crypt_v2.path}

    # Currently the only way I've found to successfully start client via systemctl.
    # Otherwise systemd-ask-password prompts for the passphrase, but the prompt
    # is never shown. https://forums.openvpn.net/viewtopic.php?f=6&t=26408
    askpass ${secrets.openvpn_odeko_askpass.path}
  '';
}
