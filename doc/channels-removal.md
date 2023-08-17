# Channels removal

## Note current channels

This is what things looked like before I switched to a flakes-based configuration:
```sh
nix-channel --list
nixos https://nixos.org/channels/nixos-unstable

l /nix/var/nix/profiles/per-user/root/
lrwxrwxrwx 1 root root 16 Apr 17 11:59 channels -> channels-10-link
lrwxrwxrwx 1 root root 60 Apr 17 11:59 channels-10-link -> /nix/store/km3qls576fksi2ivrbk0p9lz1bzamizf-user-environment
lrwxrwxrwx 1 root root 60 Dec 22  2022 channels-9-link -> /nix/store/7mki9h7nwn59vz4qr7fpfgkzpcw4axs2-user-environment

l /nix/var/nix/profiles/per-user/root/channels
lrwxrwxrwx 1 root root 16 Apr 17 11:59 /nix/var/nix/profiles/per-user/root/channels -> channels-10-link

l /nix/var/nix/profiles/per-user/root/channels-10-link
lrwxrwxrwx 1 root root 60 Apr 17 11:59 /nix/var/nix/profiles/per-user/root/channels-10-link -> /nix/store/km3qls576fksi2ivrbk0p9lz1bzamizf-user-environment

l /nix/var/nix/profiles/per-user/root/channels-10-link/
lrwxrwxrwx 1 root root 60 Dec 31  1969 manifest.nix -> /nix/store/3rj1yghv5xm6lhgra7h3ymrdj86fv38p-env-manifest.nix
lrwxrwxrwx 1 root root 55 Dec 31  1969 nixos -> /nix/store/9pa811crvq0g3ggvn65va7w0z2qzk1g7-nixos/nixos

cat /nix/var/nix/profiles/per-user/root/channels-10-link/manifest.nix
[ { meta = { }; name = "nixos"; out = { outPath = "/nix/store/9pa811crvq0g3ggvn65va7w0z2qzk1g7-nixos"; }; outPath = "/nix/store/9pa811crvq0g3ggvn65va7w0z2qzk1g7-nixos"; outputs = [ "out" ]; system = "builtin"; type = "derivation"; } ]

cat /nix/var/nix/profiles/per-user/root/channels-10-link/nixos/.git-revision
f294325aed382b66c7a188482101b0f336d1d7db

nixos-version --json
{"nixosVersion":"23.05pre474339.f294325aed3","nixpkgsRevision":"f294325aed382b66c7a188482101b0f336d1d7db"}
```

## Remove channels
TBD

## Confirm that everything still works
TBD
