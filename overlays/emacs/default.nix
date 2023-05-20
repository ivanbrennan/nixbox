let
  # NOTE: We want to pull from binary cache when possible, rather than building
  # from source, so pin emacs-overlay to a revision that was built against the
  # same nixpkgs revision we're currently using (run `nixos-version --json`).
  #
  # To that effect, I viewed the emacs-overlay jobsets listed at
  # https://hydra.nix-community.org/project/emacs-overlay
  #
  # Since I'm interested in native-comp, I viewed the unstable-gcc-pkgs jobset
  # https://hydra.nix-community.org/jobset/emacs-overlay/unstable-gcc-pkgs/evals
  #
  # I paged through the evaluations, watching the "Input changes" column for a
  # row mentioning my current nixpkgs revision. After locating it, I backtracked
  # through the subsequent (more recent) evaluations to find the most recent one
  # still using my desired nixpkgs revision.
  #
  # I clicked on the evaluation and went to its Inputs tab
  # https://hydra.nix-community.org/eval/264062#tabs-inputs
  # and grabbed the Revision from the "src" input.

  rev = "3b912580d054557e959d0b282fd12f5c473103f3";
  url = "https://github.com/nix-community/emacs-overlay/archive/${rev}.tar.gz";
in import (builtins.fetchTarball url)
