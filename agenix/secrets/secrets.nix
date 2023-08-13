let
  ivan = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM7BzvEYjobE2jwwdpJVZhqZyj6fGMuHf51IKRn05eyG ivan@agenix";
  thinkpad6 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINmp90KAZvG/C/J1/wmuMFsI5uju1v2fRQiGDguYupli root@nixos"
  thinkpad9 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINb6F08Gc0KSiEYKZcmlnS8bNNJEqTnmdTj0QlvMV07a root@nixos";
  users = [ ivan ];
  systems = [ thinkpad6 thinkpad9 ];
in
{
  "secret1.age".publicKeys = users ++ systems;
}
