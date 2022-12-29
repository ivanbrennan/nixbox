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
      nixpkgs-fly.url = "github:NixOS/nixpkgs/331c54c75a5cc8a795893c1f31524520a9dadb4d";
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
        rev = "d7a12fcc071bff59bd0ead589c975d802952a064";
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
    };

  outputs = inputs@{ self, nixpkgs, nixos-hardware, sops-nix, emacs-overlay, ... }:
    let
      system = "x86_64-linux";
      specialArgs = { inherit nixos-hardware sops-nix; };
    in {
      overlays = {
        pinned = final: prev: {
          bleep = inputs.bleep.packages.${prev.system}.bleep;
          fly = inputs.nixpkgs-fly.legacyPackages.${prev.system}.fly;
          gpick = inputs.nixpkgs-gpick.legacyPackages.${prev.system}.gpick;
          kubectl = inputs.nixpkgs-kubectl.legacyPackages.${prev.system}.kubectl;
          kubernetes-helm = (import inputs.nixpkgs-kubernetes-helm { system = prev.system; }).kubernetes-helm;
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
        flake-registry = {
          nix.registry.nixpkgs.flake = nixpkgs;
          nix.nixPath = [ "nixpkgs=flake:nixpkgs" ];
        };
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
