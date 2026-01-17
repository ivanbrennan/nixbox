{ pkgs, lib }:

let
  xmonad-session = pkgs.writeTextFile {
    name = "xmonad-xsession";
    text = ''
      [Desktop Entry]
      Version=1.0
      Type=Application
      TryExec=${pkgs.start-xsession}/bin/start-xsession
      Exec=${pkgs.start-xsession}/bin/start-xsession
      Name=XMonad
      DesktopNames=XMonad
    '';
    destination = "/share/xsessions/xmonad.desktop";
  };

  # TODO: Try Hyprland's "start-hyprland" wrapper when it becomes available.
  hyprland-session = pkgs.writeTextFile {
    name = "hyprland-session";
    text = ''
      [Desktop Entry]
      Version=1.0
      Type=Application
      TryExec=${pkgs.start-hyprland-session}/bin/start-hyprland-session
      Exec=${pkgs.start-hyprland-session}/bin/start-hyprland-session
      Name=Hyprland
      DesktopNames=Hyprland
      Keywords=tiling;wayland;compositor;
    '';
    destination = "/share/wayland-sessions/hyprland.desktop";
  };

in
  pkgs.runCommandLocal "desktop-sessions" { } ''
    mkdir -p "$out/share/"{xsessions,wayland-sessions}
    ${pkgs.buildPackages.xorg.lndir}/bin/lndir ${xmonad-session}/share/xsessions $out/share/xsessions
    ${pkgs.buildPackages.xorg.lndir}/bin/lndir ${hyprland-session}/share/wayland-sessions $out/share/wayland-sessions
  ''
