destination  = $(realpath ${machine}/default.nix)
all_machines = $(strip $(patsubst %/, %, $(dir $(wildcard */default.nix))))

symlink-machine: validate
	ln -sfr ${destination} ${CURDIR}/self.nix

validate:
ifeq (, $(filter ${machine}, ${all_machines}))
$(error Specify a valid machine: [ ${all_machines} ])
endif

.PHONY: symlink-machine validate
