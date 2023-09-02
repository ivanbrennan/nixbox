# sops-nix

To edit secrets.yaml, run the following with root privileges:
```sh
nix-shell -p sops --run "SOPS_AGE_KEY_FILE=/home/${SUDO_USER:-$USER}/.config/sops/age/keys.txt sops modules/base-configuration/secrets.yaml"
```

To rekey secrets.yaml after editing recipients in .sops.yaml, run the following with root privileges:
```sh
nix-shell -p sops --run "SOPS_AGE_KEY_FILE=/home/${SUDO_USER:-$USER}/.config/sops/age/keys.txt sops updatekeys modules/base-configuration/secrets.yaml"
```
