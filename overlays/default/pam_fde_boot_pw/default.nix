{ stdenv
, lib
, fetchFromSourcehut
, meson
, ninja
, pkg-config
, pam
, keyutils
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "pam_fde_boot_pw";
  version = "0.1";

  src = fetchFromSourcehut {
    owner = "~kennylevinsen";
    repo = "pam_fde_boot_pw";
    rev = "49bf498fd8d13f73e4a24221818a8a5d2af20088";
    hash = "sha256-dS9ufryg3xfxgUzJKDgrvMZP2qaYH+WJQFw1ogl1isc=";
  };

  nativeBuildInputs = [
    meson
    ninja
    pkg-config
  ];

  buildInputs = [
    pam
    keyutils
  ];

  mesonFlags = [
    (lib.mesonOption "pam-mod-dir" "${placeholder "out"}/lib/security")
  ];

  meta = with lib; {
    description = "PAM module for leveraging disk encryption password in the PAM session";
    longDescription = ''
      pam_fde_boot_pw transfers passwords stored by early full disk encryption
      unlock from the kernel keyring to the PAM session, allowing user-space
      keyrings like gkr and kwallet5 to be automatically unlocked without
      additional password authentication.

      Assumes that the key will be stored as "cryptsetup".

      Difference from pam_systemd_loadkey and pam_gdm:
      pam_systemd_loadkey and pam_gdm both attempt to read out the key when
      authentication is attempted, letting authentication succeed without asking
      password questions if the credentials are present and valid.

      This does not work in the case where auto-login does not attempt
      authentication first. Instead, this module works on the request to open
      the session.
    '';
    homepage = "https://git.sr.ht/~kennylevinsen/pam_fde_boot_pw";
    license = licenses.mit;
    maintainers = with maintainers; [ ivanbrennan ];
    platforms = platforms.linux;
  };
})
