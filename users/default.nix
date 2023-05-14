{
  users = {
    users.ivan = {
      isNormalUser = true;
      uid = 1000;
      createHome = true;
      home = "/home/ivan";
      extraGroups = [
        "dialout"
        "docker"
        "input"
        "libvirtd"
        "networkmanager"
        "uinput"
        "vboxusers"
        "video"
        "wheel"
        "wireshark"
      ];
      hashedPassword = "$y$j9T$vowJiSYMEds55yVoU3Rwn0$tdRX1zSuZRd8I.NJJwzYVEGKQn.ATGPXmH/ZubzmbE4";
    };

    mutableUsers = false;
  };
}
