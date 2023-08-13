let
  ivan = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM7BzvEYjobE2jwwdpJVZhqZyj6fGMuHf51IKRn05eyG ivan@agenix";
  thinkpad9 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINb6F08Gc0KSiEYKZcmlnS8bNNJEqTnmdTj0QlvMV07a root@nixos";
  users = [ ivan ];
  systems = [ thinkpad9 ];
in
{
  "secret1.age".publicKeys = users ++ systems;
}
