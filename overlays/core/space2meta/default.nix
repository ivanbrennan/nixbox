{ stdenv, fetchurl, cmake }:

let
  version = "0.1.0";
  pname = "interception-tools-space2meta";
in stdenv.mkDerivation {
  name = "${pname}-${version}";

  src = fetchurl {
    url = "https://gitlab.com/interception/linux/plugins/space2meta/repository/v${version}/archive.tar.gz";
    sha256 = "1cx0sjx5vmfps865ya1cscr3cpd78a75nszais7g8hzmgx0wgfd1";
  };

  buildInputs = [ cmake ];

  meta = with stdenv.lib; {
    homepage = "https://gitlab.com/interception/linux/plugins/space2meta";
    description = "Turn space into meta when chorded to another key (on release)";
    license = licenses.mit;
    maintainers = [ maintainers.ivanbrennan ];
    platforms = platforms.linux;
  };
}
