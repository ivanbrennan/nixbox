machine = $(shell hostname)

all: symlink-machine set-channel

symlink-machine: validate-user
	make -C machines machine=${machine}

set-channel: validate-user
	nix-channel --add https://nixos.org/channels/nixos-unstable nixos
	nix-channel --update

validate-user:
ifneq (root, ${USER})
$(error This should be run as root)
endif

.PHONY: all symlink-machine set-channel validate-user
