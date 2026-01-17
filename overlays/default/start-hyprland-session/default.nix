{ writeShellScriptBin
, hyprland
, dbus
, lib
, gnome-keyring
}:

let
  hyprland-session = writeShellScriptBin "hyprland-session" ''
    # Connect to the gnome-keyring-daemon that was started by PAM on login.
    eval $(${gnome-keyring}/bin/gnome-keyring-daemon --start --components=secrets)

    # Hack: https://bugzilla.redhat.com/show_bug.cgi?id=2250704 still
    # applies to sessions not managed by systemd.
    if [ -z "$SSH_AUTH_SOCK" ] && [ -n "$XDG_RUNTIME_DIR" ]
    then
      export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/gcr/ssh"
    fi

    export XDG_CURRENT_DESKTOP=Hyprland
    export XDG_SESSION_DESKTOP=hyprland
    export XDG_SESSION_TYPE=wayland
    ${lib.getBin dbus}/bin/dbus-update-activation-environment --systemd --all

    exec ${hyprland}/bin/Hyprland
  '';
in
  writeShellScriptBin "start-hyprland-session" ''
    exec /run/current-system/systemd/bin/systemd-cat --identifier=hyprland-session -- ${hyprland-session}/bin/hyprland-session
  ''
