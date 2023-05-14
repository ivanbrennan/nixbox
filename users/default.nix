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
        "networkmanager"
        "uinput"
        "vboxusers"
        "video"
        "wheel"
        "wireshark"
      ];
      hashedPassword = "$y$j9T$vowJiSYMEds55yVoU3Rwn0$tdRX1zSuZRd8I.NJJwzYVEGKQn.ATGPXmH/ZubzmbE4";
      openssh.authorizedKeys.keys = [
        ''
          from="192.168.0.0/24" ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDABjGqcHsTTDjmT30YUZ9VUJMz0cNYFqIRROz/7NVmS79gvIeS4/ll+flOtdgVcsDijjghHqA9AM/4OCv5sKICaufRV73PS4HKk06yfiCS2au5YzIg/jd+7gK5smxpS+55qtR0Yu1hOBrBik0Q2J7biLNpXLqHLnnrrrS5mkgnIRAb7Ojv/CQKT+ZDcusJWsZ7pzxY1BHqC59VNuy79knVbPAE44n6jnIXlfcIACVqmHlU/W6KVvxfkv+lncf2t6SAj3AuWdFD98YuWxN5QlGBPe+If5WwneYUc3ENjiSAJu1sHUYU9BMhe9YEFiCZVzKsv45Lr+1HlA225u447835 ivan.brennan@gmail.com
        ''
      ];
    };

    mutableUsers = false;
  };
}
