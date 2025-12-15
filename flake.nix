{
  description = "NixOS configuration";

  inputs =
    {
      nixpkgs = {
        type = "github";
        owner = "NixOS";
        repo = "nixpkgs";
        ref = "nixos-unstable";
      };

      # Pinned nixpkgs revisions for specific package versions.
      nixpkgs-fly.url = "github:NixOS/nixpkgs/04ba740f89b23001077df136c216a885371533ad";
      nixpkgs-gpick.url = "github:NixOS/nixpkgs/d2042f91c1ad953825556be3ec2e53cf9a91fc77";
      nixpkgs-kubectl.url = "github:NixOS/nixpkgs/fe2ecaf706a5907b5e54d979fbde4924d84b65fc";
      nixpkgs-kubernetes-helm = {
        type = "github";
        owner = "NixOS";
        repo = "nixpkgs";
        rev = "a071bfa7e7bbd62e1b43830e5e79d8b36afe5fa6";
        flake = false;
      };

      nixos-hardware = {
        type = "github";
        owner = "NixOS";
        repo = "nixos-hardware";
      };

      sops-nix = {
        type = "github";
        owner = "Mic92";
        repo = "sops-nix";
        rev = "c36df4fe4bf4bb87759b1891cab21e7a05219500";
        inputs.nixpkgs.follows = "nixpkgs";
        inputs.nixpkgs-stable.follows = ""; # don't download test deps
      };

      bleep = {
        type = "github";
        owner = "ivanbrennan";
        repo = "bleep";
        inputs.nixpkgs.follows = "nixpkgs";
      };

      emacs-overlay = {
        type = "github";
        owner = "nix-community";
        repo = "emacs-overlay";
        rev = "3b912580d054557e959d0b282fd12f5c473103f3";
        # inputs.nixpkgs.follows = "nixpkgs";
      };

      docspell = {
        type = "github";
        owner = "ivanbrennan";
        repo = "docspell";
        rev = "baf5c682b0d78717e42352fe6525e10fb01ab836";
        inputs.nixpkgs.follows = "nixpkgs";
      };
    };

  outputs = inputs@{ self, nixpkgs, nixos-hardware, sops-nix, emacs-overlay, docspell, ... }:
    let
      system = "x86_64-linux";
      specialArgs = { inherit nixos-hardware sops-nix docspell; };
    in {
      overlays = {
        pinned = final: prev:
          let
            prevSystem = prev.stdenv.hostPlatform.system;
          in {
            bleep = inputs.bleep.packages.${prevSystem}.bleep;
            fly = inputs.nixpkgs-fly.legacyPackages.${prevSystem}.fly;
            gpick = inputs.nixpkgs-gpick.legacyPackages.${prevSystem}.gpick;
            kubectl = inputs.nixpkgs-kubectl.legacyPackages.${prevSystem}.kubectl;
            kubernetes-helm = (import inputs.nixpkgs-kubernetes-helm { system = prevSystem; }).kubernetes-helm;
            docspell-restserver = inputs.docspell.packages.${prevSystem}.docspell-restserver;
            docspell-joex = inputs.docspell.packages.${prevSystem}.docspell-joex;
          };
        default = import ./overlays/default;
        haskell = import ./overlays/haskell;
        vim     = import ./overlays/vim;
        emacs   = emacs-overlay.overlays.default;
      };

      nixosModules = {
        default = ./modules/base-configuration;
        use-overlays = {
          nixpkgs.overlays = builtins.attrValues self.overlays;
        };
        flake-nixpkgs = {
          # Despite what I thought in commit bee2c84429262510d95a4413c485fc4b9c296907,
          # removing the following seems to result in nixpkgs being absent from NIX_PATH.
          # I suspect this is because the automatic configuration I mentioned in that
          # commit only sets the default value, which I then override in base-configuration.
          # https://discourse.nixos.org/t/24-05-add-flake-to-nix-path/46310/2
          nix.nixPath = [ "nixpkgs=flake:nixpkgs" ];
        };
        # docspell-configuration = ./modules/docspell-configuration;
      };

      nixosConfigurations.thinkpad9 = nixpkgs.lib.nixosSystem {
        inherit system specialArgs;
        modules =
          [ ./hosts/thinkpad9/configuration.nix ]
          ++ builtins.attrValues self.nixosModules;
      };

      nixosConfigurations.thinkpad6 = nixpkgs.lib.nixosSystem {
        inherit system specialArgs;
        modules =
          [ ./hosts/thinkpad6/configuration.nix ]
          ++ builtins.attrValues self.nixosModules;
      };

      nixosConfigurations.bigThinkPad = nixpkgs.lib.nixosSystem {
        inherit system specialArgs;
        modules =
          [ ./hosts/bigThinkPad/configuration.nix ]
          ++ builtins.attrValues self.nixosModules;
      };

      nixosConfigurations.littleThinkPad = nixpkgs.lib.nixosSystem {
        inherit system specialArgs;
        modules =
          [ ./hosts/littleThinkPad/configuration.nix ]
          ++ builtins.attrValues self.nixosModules;
      };

      nixosConfigurations.nixosbox = nixpkgs.lib.nixosSystem {
        inherit system specialArgs;
        modules =
          [ ./hosts/nixosbox/configuration.nix ]
          ++ builtins.attrValues self.nixosModules;
      };
    };
}
