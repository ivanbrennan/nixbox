# https://github.com/LSLeary/haskell-overlays
self: super: {
  haskellPackages = super.haskellPackages.override {
    overrides = import ./performOverlay.nix self super [
      ./xmonad-contrib-hoverlay.nix
      ./xmonad-contrib-nohaddock-hoverlay.nix
    ];
  };
}
