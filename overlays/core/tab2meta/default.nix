{ stdenv, fetchurl, cmake }:

let
  version = "0.1.0";
  pname = "interception-tools-tab2meta";
in stdenv.mkDerivation {
  name = "${pname}-${version}";

  src = fetchurl {
    url = "https://gitlab.com/ivanbrennan/tab2meta/repository/aa83aeeefa47926f779897987d205c9a373380b7/archive.tar.gz";
    sha256 = "05r8253nlg93vkaqsyk23y8vri8dac6ypakp4z2dn4zaqcf8i8yj";
  };

  buildInputs = [ cmake ];

  meta = with stdenv.lib; {
    homepage = "https://gitlab.com/ivanbrennan/tab2meta";
    description = "Turn tab into meta when chorded to another key (on release)";
    maintainers = [ maintainers.ivanbrennan ];
    platforms = platforms.linux;
  };
}
