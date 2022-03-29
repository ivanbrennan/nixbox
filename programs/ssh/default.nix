{
  knownHosts =
    {
      "github.com" = {
        publicKeyFile = ./pubkeys/github_ssh_host_rsa_key.pub;
      };
    };

  extraConfig = ''
    Host 192.168.0.*
      ForwardAgent yes
  '';
}
