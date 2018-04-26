{ lib, bundlerEnv, ruby }:

bundlerEnv rec {
  name = "interactive-editor-0.0.11";

  version = (import ./gemset).interactive-editor.version;
  inherit ruby;
  # expects Gemfile, Gemfile.lock and gemset.nix in the same directory
  gemdir = ./.;

  meta = with lib; {
    description = "Interactive editing in irb";
    homepage    = http://github.com/jberkel/interactive_editor;
    license     = with licenses; mit;
    platforms   = platforms.unix;
  };
}
