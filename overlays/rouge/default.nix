{ lib, bundlerApp }:

bundlerApp {
  pname = "rouge";
  gemdir = ./.;
  exes = [ "rougify" ];

  meta = with lib; {
    description     = "A pure-ruby colorizer based on pygments";
    longDescription = "Rouge aims to a be a simple, easy-to-extend drop-in replacement for pygments.";
    homepage        = http://rouge.jneen.net/;
    license         = licenses.mit;
    platforms       = platforms.unix;
  };
}
