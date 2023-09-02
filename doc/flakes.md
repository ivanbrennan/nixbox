# Flakes

## Flakes-based configurations

https://github.com/Misterio77/nix-starter-configs/tree/6c374a2824a4886922c9ccf8a5dfd505d23bd4cf
https://github.com/Misterio77/nix-config/tree/5d4e7099910905708029a2fc52eb321644af6ac3
https://github.com/Mic92/dotfiles/tree/276d0aa2444895f0fb3f56e12d6fbcab7d8bddfc
https://github.com/hlissner/dotfiles/tree/089f1a9da9018df9e5fc200c2d7bef70f4546026

## Misc references

https://github.com/NixOS/templates/blob/0fb94bf87144b18e42765693c3e15ac5f17eeab0/full/flake.nix
https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html#flake-inputs
https://nixos-and-flakes.thiscute.world

## Inspect configuration with nix eval

Example:
```sh
nix eval '/etc/nixos#nixosConfigurations.thinkpad9.config.nix.nixPath'
```
