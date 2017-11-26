{
  extraUsers.ivan = {
    isNormalUser = true;
    uid = 1000;
    createHome = true;
    home = "/home/ivan";
    extraGroups = [
      "wheel"
      "networkmanager"
    ];
    hashedPassword = "$6$4uOYQEuFA$RqNmGNfQcR6mPK2.jSHPntF43HgN6BJP4nwQANNUbp8ulpquniQNqecgUMVGRBsjBzt2b7gJBhCCedbUYmI/60";
  };

  mutableUsers = false;
}
