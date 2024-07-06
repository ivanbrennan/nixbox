{ config, lib, pkgs, docspell, ... }:

{
  imports = [ docspell.nixosModules.default ];

  services.docspell-joex = {
    enable = true;
    configFile = config.sops.secrets.docspell_joex_config.path;
  };

  services.docspell-restserver = {
    enable = true;
    configFile = config.sops.secrets.docspell_restserver_config.path;
  };

  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_15;
    enableTCPIP = true;
    initialScript = config.sops.secrets.docspell_pginit.path;
    settings = {
      port = 5432;
      ssl = true;
      ssl_cert_file = config.sops.secrets.postgresql_ssl_cert.path;
      ssl_key_file = config.sops.secrets.postgresql_ssl_key.path;
    };
    authentication = lib.mkForce ''
      # TYPE    DATABASE   USER   ADDRESS     METHOD
      hostssl   all        all    0.0.0.0/0   scram-sha-256
      hostssl   all        all    ::1/128     scram-sha-256
      local     all        all                peer
    '';
  };

  networking = {
    firewall.allowedTCPPorts = [7880];
  };
}
