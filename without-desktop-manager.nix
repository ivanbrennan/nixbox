{ config, lib, pkgs, ... }:

let
  mimeAppsList = pkgs.writeTextFile {
    name = "gnome-mimeapps";
    destination = "/share/applications/mimeapps.list";
    text = ''
      [Default Applications]
      inode/directory=nautilus.desktop;org.gnome.Nautilus.desktop
    '';
  };
in

{
  services.xserver.desktopManager.gnome3.enable = false;

  hardware.bluetooth.enable = true;
  hardware.pulseaudio.enable = true;
  programs.dconf.enable = true;
  security.polkit.enable = true;
  services.dleyna-renderer.enable = true;
  services.dleyna-server.enable = true;
  services.gnome3.gnome-keyring.enable = true;
  services.hardware.bolt.enable = true;
  services.packagekit.enable = true;
  services.udisks2.enable = true;
  services.upower.enable = config.powerManagement.enable;

  xdg.portal.enable = true;
  xdg.portal.extraPortals = [
    pkgs.xdg-desktop-portal-gtk
  ];

  networking.networkmanager.enable = true;

  services.xserver.updateDbusEnvironment = true;

  # gnome has a custom alert theme but it still
  # inherits from the freedesktop theme.
  environment.systemPackages = with pkgs; [
    sound-theme-freedesktop
  ];

  # Needed for themes and backgrounds
  environment.pathsToLink = [
    "/share" # TODO: https://github.com/NixOS/nixpkgs/issues/47173
    "/share/nautilus-python/extensions"
  ];

  services.colord.enable = true;
  services.gnome3.glib-networking.enable = true;
  services.gnome3.gnome-user-share.enable = true;
  services.gnome3.rygel.enable = true;
  services.gvfs.enable = true;
  services.telepathy.enable = true;

  services.udev.packages = with pkgs.gnome3; [
    # Force enable KMS modifiers for devices that require them.
    # https://gitlab.gnome.org/GNOME/mutter/-/merge_requests/1443
    mutter
  ];

  programs.seahorse.enable = true;

  # Let nautilus find extensions
  # TODO: Create nautilus-with-extensions package
  environment.sessionVariables.NAUTILUS_EXTENSION_DIR = "${config.system.path}/lib/nautilus/extensions-3.0";

  # Prioritize nautilus by default when opening directories
  environment.sessionVariables.XDG_DATA_DIRS = [ "${mimeAppsList}/share" ];
}
