{ lib, bundlerApp }:

bundlerApp {
  pname = "coderay";
  gemdir = ./.;
  exes = [ "coderay" ];

  meta = with lib; {
    description     = "Fast syntax highlighting for selected languages";
    longDescription = "Fast and easy syntax highlighting for selected languages, written in Ruby. Comes with RedCloth integration and LOC counter.";
    homepage        = http://coderay.rubychan.de/;
    license         = licenses.mit;
    platforms       = platforms.unix;
  };
}
