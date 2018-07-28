# generated using pypi2nix tool (version: 1.8.1)
# See more at: https://github.com/garbas/pypi2nix
#
# COMMAND:
#   pypi2nix -V 2.7 -r requirements.txt -E libffi -E openssl
#

{ pkgs ? import <nixpkgs> {}
}:

let

  inherit (pkgs) makeWrapper;
  inherit (pkgs.stdenv.lib) fix' extends inNixShell;

  pythonPackages =
  import "${toString pkgs.path}/pkgs/top-level/python-packages.nix" {
    inherit pkgs;
    inherit (pkgs) stdenv;
    python = pkgs.python27Full;
    # patching pip so it does not try to remove files when running nix-shell
    overrides =
      self: super: {
        bootstrapped-pip = super.bootstrapped-pip.overrideDerivation (old: {
          patchPhase = old.patchPhase + ''
            sed -i               -e "s|paths_to_remove.remove(auto_confirm)|#paths_to_remove.remove(auto_confirm)|"                -e "s|self.uninstalled = paths_to_remove|#self.uninstalled = paths_to_remove|"                  $out/${pkgs.python35.sitePackages}/pip/req/req_install.py
          '';
        });
      };
  };

  commonBuildInputs = with pkgs; [ libffi openssl ];
  commonDoCheck = false;

  withPackages = pkgs':
    let
      pkgs = builtins.removeAttrs pkgs' ["__unfix__"];
      interpreter = pythonPackages.buildPythonPackage {
        name = "python27Full-interpreter";
        buildInputs = [ makeWrapper ] ++ (builtins.attrValues pkgs);
        buildCommand = ''
          mkdir -p $out/bin
          ln -s ${pythonPackages.python.interpreter}               $out/bin/${pythonPackages.python.executable}
          for dep in ${builtins.concatStringsSep " "               (builtins.attrValues pkgs)}; do
            if [ -d "$dep/bin" ]; then
              for prog in "$dep/bin/"*; do
                if [ -f $prog ]; then
                  ln -s $prog $out/bin/`basename $prog`
                fi
              done
            fi
          done
          for prog in "$out/bin/"*; do
            wrapProgram "$prog" --prefix PYTHONPATH : "$PYTHONPATH"
          done
          pushd $out/bin
          ln -s ${pythonPackages.python.executable} python
          ln -s ${pythonPackages.python.executable}               python2
          popd
        '';
        passthru.interpreter = pythonPackages.python;
      };
    in {
      __old = pythonPackages;
      inherit interpreter;
      mkDerivation = pythonPackages.buildPythonPackage;
      packages = pkgs;
      overrideDerivation = drv: f:
        pythonPackages.buildPythonPackage (drv.drvAttrs // f drv.drvAttrs //                                            { meta = drv.meta; });
      withPackages = pkgs'':
        withPackages (pkgs // pkgs'');
    };

  python = withPackages {};

  generated = self: {

    "Jinja2" = python.mkDerivation {
      name = "Jinja2-2.10";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/56/e6/332789f295cf22308386cf5bbd1f4e00ed11484299c5d7383378cf48ba47/Jinja2-2.10.tar.gz"; sha256 = "f84be1bb0040caca4cea721fcbbbbd61f9be9464ca236387158b0feea01914a4"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [
      self."MarkupSafe"
    ];
      meta = with pkgs.stdenv.lib; {
        homepage = "http://jinja.pocoo.org/";
        license = licenses.bsdOriginal;
        description = "A small but fast and easy to use stand-alone template engine written in pure python.";
      };
    };



    "MarkupSafe" = python.mkDerivation {
      name = "MarkupSafe-1.0";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/4d/de/32d741db316d8fdb7680822dd37001ef7a448255de9699ab4bfcbdf4172b/MarkupSafe-1.0.tar.gz"; sha256 = "a6be69091dac236ea9c6bc7d012beab42010fa914c459791d627dad4910eb665"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "http://github.com/pallets/markupsafe";
        license = licenses.bsdOriginal;
        description = "Implements a XML/HTML/XHTML Markup safe string for Python";
      };
    };



    "PyYAML" = python.mkDerivation {
      name = "PyYAML-3.13";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/9e/a3/1d13970c3f36777c583f136c136f804d70f500168edc1edea6daa7200769/PyYAML-3.13.tar.gz"; sha256 = "3ef3092145e9b70e3ddd2c7ad59bdd0252a94dfe3949721633e41344de00a6bf"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "http://pyyaml.org/wiki/PyYAML";
        license = licenses.mit;
        description = "YAML parser and emitter for Python";
      };
    };



    "ansicolors" = python.mkDerivation {
      name = "ansicolors-1.1.8";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/76/31/7faed52088732704523c259e24c26ce6f2f33fbeff2ff59274560c27628e/ansicolors-1.1.8.zip"; sha256 = "99f94f5e3348a0bcd43c82e5fc4414013ccc19d70bd939ad71e0133ce9c372e0"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "http://github.com/jonathaneunice/colors/";
        license = licenses.isc;
        description = "ANSI colors for Python";
      };
    };



    "argcomplete" = python.mkDerivation {
      name = "argcomplete-1.9.4";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/3c/21/9741e5e5e63245a8cdafb32ffc738bff6e7ef6253b65953e77933e56ce88/argcomplete-1.9.4.tar.gz"; sha256 = "06c8a54ffaa6bfc9006314498742ec8843601206a3b94212f82657673662ecf1"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/kislyuk/argcomplete";
        license = "License :: OSI Approved :: Apache Software License";
        description = "Bash tab completion for argparse";
      };
    };



    "argparse" = python.mkDerivation {
      name = "argparse-1.4.0";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/18/dd/e617cfc3f6210ae183374cd9f6a26b20514bbb5a792af97949c5aacddf0f/argparse-1.4.0.tar.gz"; sha256 = "62b089a55be1d8949cd2bc7e0df0bddb9e028faefc8c32038cc84862aefdd6e4"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/ThomasWaldmann/argparse/";
        license = licenses.psfl;
        description = "Python command-line parsing library";
      };
    };



    "asn1crypto" = python.mkDerivation {
      name = "asn1crypto-0.24.0";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/fc/f1/8db7daa71f414ddabfa056c4ef792e1461ff655c2ae2928a2b675bfed6b4/asn1crypto-0.24.0.tar.gz"; sha256 = "9d5c20441baf0cb60a4ac34cc447c6c189024b6b4c6cd7877034f4965c464e49"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/wbond/asn1crypto";
        license = licenses.mit;
        description = "Fast ASN.1 parser and serializer with definitions for private keys, public keys, certificates, CRL, OCSP, CMS, PKCS#3, PKCS#7, PKCS#8, PKCS#12, PKCS#5, X.509 and TSP";
      };
    };



    "atomicwrites" = python.mkDerivation {
      name = "atomicwrites-1.1.5";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/a1/e1/2d9bc76838e6e6667fde5814aa25d7feb93d6fa471bf6816daac2596e8b2/atomicwrites-1.1.5.tar.gz"; sha256 = "240831ea22da9ab882b551b31d4225591e5e447a68c5e188db5b89ca1d487585"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/untitaker/python-atomicwrites";
        license = licenses.mit;
        description = "Atomic file writes.";
      };
    };



    "attrs" = python.mkDerivation {
      name = "attrs-18.1.0";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/e4/ac/a04671e118b57bee87dabca1e0f2d3bda816b7a551036012d0ca24190e71/attrs-18.1.0.tar.gz"; sha256 = "e0d0eb91441a3b53dab4d9b743eafc1ac44476296a2053b6ca3af0b139faf87b"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [
      self."pytest"
      self."six"
    ];
      meta = with pkgs.stdenv.lib; {
        homepage = "http://www.attrs.org/";
        license = licenses.mit;
        description = "Classes Without Boilerplate";
      };
    };



    "backports.ssl-match-hostname" = python.mkDerivation {
      name = "backports.ssl-match-hostname-3.5.0.1";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/76/21/2dc61178a2038a5cb35d14b61467c6ac632791ed05131dda72c20e7b9e23/backports.ssl_match_hostname-3.5.0.1.tar.gz"; sha256 = "502ad98707319f4a51fa2ca1c677bd659008d27ded9f6380c79e8932e38dcdf2"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "http://bitbucket.org/brandon/backports.ssl_match_hostname";
        license = licenses.psfl;
        description = "The ssl.match_hostname() function from Python 3.5";
      };
    };



    "boto" = python.mkDerivation {
      name = "boto-2.49.0";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/c8/af/54a920ff4255664f5d238b5aebd8eedf7a07c7a5e71e27afcfe840b82f51/boto-2.49.0.tar.gz"; sha256 = "ea0d3b40a2d852767be77ca343b58a9e3a4b00d9db440efb8da74b4e58025e5a"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/boto/boto/";
        license = licenses.mit;
        description = "Amazon Web Services Library";
      };
    };



    "boto3" = python.mkDerivation {
      name = "boto3-1.6.7";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/70/58/74c5119bb9e0162c7fbe8f12e6c4dc5715fa090bf3462010b7cd55f7bf6f/boto3-1.6.7.tar.gz"; sha256 = "3a74aa62eb9c0a6e1c9eace4ab93b2c3de43ab9f116dbc2e60a8cee584532584"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [
      self."botocore"
      self."jmespath"
      self."s3transfer"
    ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/boto/boto3";
        license = licenses.asl20;
        description = "The AWS SDK for Python";
      };
    };



    "botocore" = python.mkDerivation {
      name = "botocore-1.9.7";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/6b/66/211206337f8f34a98b340499fc18859eb5b2428c66c0961df97c249354f3/botocore-1.9.7.tar.gz"; sha256 = "0794994bf5a50d1b7df0b43c319d4a2ecd3fe26b55c0313e7427c0910fc977c1"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [
      self."docutils"
      self."jmespath"
      self."python-dateutil"
    ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/boto/botocore";
        license = licenses.asl20;
        description = "Low-level, data-driven core of boto 3.";
      };
    };



    "cachetools" = python.mkDerivation {
      name = "cachetools-2.1.0";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/87/41/b3e00059f3c34b57a653d2120d213715abb4327b36fee22e59c1da977d25/cachetools-2.1.0.tar.gz"; sha256 = "90f1d559512fc073483fe573ef5ceb39bf6ad3d39edc98dc55178a2b2b176fa3"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/tkem/cachetools";
        license = licenses.mit;
        description = "Extensible memoizing collections and decorators";
      };
    };



    "certifi" = python.mkDerivation {
      name = "certifi-2018.4.16";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/4d/9c/46e950a6f4d6b4be571ddcae21e7bc846fcbb88f1de3eff0f6dd0a6be55d/certifi-2018.4.16.tar.gz"; sha256 = "13e698f54293db9f89122b0581843a782ad0934a4fe0172d2a980ba77fc61bb7"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "http://certifi.io/";
        license = licenses.mpl20;
        description = "Python package for providing Mozilla's CA Bundle.";
      };
    };



    "cffi" = python.mkDerivation {
      name = "cffi-1.11.5";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/e7/a7/4cd50e57cc6f436f1cc3a7e8fa700ff9b8b4d471620629074913e3735fb2/cffi-1.11.5.tar.gz"; sha256 = "e90f17980e6ab0f3c2f3730e56d1fe9bcba1891eeea58966e89d352492cc74f4"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [
      self."pycparser"
    ];
      meta = with pkgs.stdenv.lib; {
        homepage = "http://cffi.readthedocs.org";
        license = licenses.mit;
        description = "Foreign Function Interface for Python calling C code.";
      };
    };



    "colorama" = python.mkDerivation {
      name = "colorama-0.3.9";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/e6/76/257b53926889e2835355d74fec73d82662100135293e17d382e2b74d1669/colorama-0.3.9.tar.gz"; sha256 = "48eb22f4f8461b1df5734a074b57042430fb06e1d61bd1e11b078c0fe6d7a1f1"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/tartley/colorama";
        license = licenses.bsdOriginal;
        description = "Cross-platform colored terminal text.";
      };
    };



    "credstash" = python.mkDerivation {
      name = "credstash-1.13.4";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/75/89/a8baf6b08c097a30dbe2e70b130cd3ba55a131195ec88c9a302a208eebee/credstash-1.13.4.tar.gz"; sha256 = "676cc03a6143dec260c78aeef09d08a64faf27c411f8a94f6d9338e985879f81"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [
      self."PyYAML"
      self."boto3"
      self."cryptography"
    ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/LuminalOSS/credstash";
        license = "License :: OSI Approved :: Apache Software License";
        description = "A utility for managing secrets in the cloud using AWS KMS and DynamoDB";
      };
    };



    "cryptography" = python.mkDerivation {
      name = "cryptography-2.0.3";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/9c/1a/0fc8cffb04582f9ffca61b15b0681cf2e8588438e55f61403eb9880bd8e0/cryptography-2.0.3.tar.gz"; sha256 = "d04bb2425086c3fe86f7bc48915290b13e798497839fbb18ab7f6dffcf98cc3a"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [
      self."asn1crypto"
      self."cffi"
      self."enum34"
      self."idna"
      self."ipaddress"
      self."pytest"
      self."six"
    ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/pyca/cryptography";
        license = licenses.bsdOriginal;
        description = "cryptography is a package which provides cryptographic recipes and primitives to Python developers.";
      };
    };



    "docutils" = python.mkDerivation {
      name = "docutils-0.14";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/84/f4/5771e41fdf52aabebbadecc9381d11dea0fa34e4759b4071244fa094804c/docutils-0.14.tar.gz"; sha256 = "51e64ef2ebfb29cae1faa133b3710143496eca21c530f3f71424d77687764274"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "http://docutils.sourceforge.net/";
        license = licenses.publicDomain;
        description = "Docutils -- Python Documentation Utilities";
      };
    };



    "enum34" = python.mkDerivation {
      name = "enum34-1.1.6";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/bf/3e/31d502c25302814a7c2f1d3959d2a3b3f78e509002ba91aea64993936876/enum34-1.1.6.tar.gz"; sha256 = "8ad8c4783bf61ded74527bffb48ed9b54166685e4230386a9ed9b1279e2df5b1"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://bitbucket.org/stoneleaf/enum34";
        license = licenses.bsdOriginal;
        description = "Python 3.4 Enum backported to 3.3, 3.2, 3.1, 2.7, 2.6, 2.5, and 2.4";
      };
    };



    "funcsigs" = python.mkDerivation {
      name = "funcsigs-1.0.2";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/94/4a/db842e7a0545de1cdb0439bb80e6e42dfe82aaeaadd4072f2263a4fbed23/funcsigs-1.0.2.tar.gz"; sha256 = "a7bb0f2cf3a3fd1ab2732cb49eba4252c2af4240442415b4abce3b87022a8f50"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "http://funcsigs.readthedocs.org";
        license = "License :: OSI Approved :: Apache Software License";
        description = "Python function signatures from PEP362 for Python 2.6, 2.7 and 3.2+";
      };
    };



    "futures" = python.mkDerivation {
      name = "futures-3.2.0";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/1f/9e/7b2ff7e965fc654592269f2906ade1c7d705f1bf25b7d469fa153f7d19eb/futures-3.2.0.tar.gz"; sha256 = "9ec02aa7d674acb8618afb127e27fde7fc68994c0437ad759fa094a574adb265"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/agronholm/pythonfutures";
        license = licenses.psfl;
        description = "Backport of the concurrent.futures package from Python 3";
      };
    };



    "google-auth" = python.mkDerivation {
      name = "google-auth-1.5.0";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/a2/39/0c3d41aa96c1bdb9dcc4f646909b82e4c0ed9a1a398e869627a21a0563c4/google-auth-1.5.0.tar.gz"; sha256 = "1745c9066f698eac3da99cef082914495fb71bc09597ba7626efbbb64c4acc57"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [
      self."cachetools"
      self."pyasn1-modules"
      self."rsa"
      self."six"
    ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/GoogleCloudPlatform/google-auth-library-python";
        license = licenses.asl20;
        description = "Google Authentication Library";
      };
    };



    "html5lib" = python.mkDerivation {
      name = "html5lib-1.0.1";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/85/3e/cf449cf1b5004e87510b9368e7a5f1acd8831c2d6691edd3c62a0823f98f/html5lib-1.0.1.tar.gz"; sha256 = "66cb0dcfdbbc4f9c3ba1a63fdb511ffdbd4f513b2b6d81b80cd26ce6b3fb3736"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [
      self."six"
      self."webencodings"
    ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/html5lib/html5lib-python";
        license = licenses.mit;
        description = "HTML parser based on the WHATWG HTML specification";
      };
    };



    "idna" = python.mkDerivation {
      name = "idna-2.7";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/65/c4/80f97e9c9628f3cac9b98bfca0402ede54e0563b56482e3e6e45c43c4935/idna-2.7.tar.gz"; sha256 = "684a38a6f903c1d71d6d5fac066b58d7768af4de2b832e426ec79c30daa94a16"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/kjd/idna";
        license = licenses.bsdOriginal;
        description = "Internationalized Domain Names in Applications (IDNA)";
      };
    };



    "ipaddress" = python.mkDerivation {
      name = "ipaddress-1.0.22";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/97/8d/77b8cedcfbf93676148518036c6b1ce7f8e14bf07e95d7fd4ddcb8cc052f/ipaddress-1.0.22.tar.gz"; sha256 = "b146c751ea45cad6188dd6cf2d9b757f6f4f8d6ffb96a023e6f2e26eea02a72c"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/phihag/ipaddress";
        license = licenses.psfl;
        description = "IPv4/IPv6 manipulation library";
      };
    };



    "jmespath" = python.mkDerivation {
      name = "jmespath-0.9.3";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/e5/21/795b7549397735e911b032f255cff5fb0de58f96da794274660bca4f58ef/jmespath-0.9.3.tar.gz"; sha256 = "6a81d4c9aa62caf061cb517b4d9ad1dd300374cd4706997aff9cd6aedd61fc64"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/jmespath/jmespath.py";
        license = licenses.mit;
        description = "JSON Matching Expressions";
      };
    };



    "kubernetes" = python.mkDerivation {
      name = "kubernetes-6.0.0";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/e1/32/061ac1fa026c63321c5da3488024f0bc70d74d22e3b0d51c5e5afbe2b701/kubernetes-6.0.0.tar.gz"; sha256 = "b370ab4abd925309db69a14a4723487948e9a83de60ca92782ec14992b741c89"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [
      self."PyYAML"
      self."certifi"
      self."google-auth"
      self."ipaddress"
      self."python-dateutil"
      self."requests"
      self."requests-oauthlib"
      self."six"
      self."urllib3"
      self."websocket-client"
    ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/kubernetes-client/python";
        license = licenses.asl20;
        description = "Kubernetes python client";
      };
    };



    "marshmallow" = python.mkDerivation {
      name = "marshmallow-2.15.0";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/dc/34/b99d68c78378783c96254cebdf82b6ffa887a1f0e955ec5362eff2f1b2c2/marshmallow-2.15.0.tar.gz"; sha256 = "d3f31fe7be2106b1d783cbd0765ef4e1c6615505514695f33082805f929dd584"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [
      self."python-dateutil"
    ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/marshmallow-code/marshmallow";
        license = licenses.mit;
        description = "A lightweight library for converting complex datatypes to and from native Python datatypes.";
      };
    };



    "more-itertools" = python.mkDerivation {
      name = "more-itertools-4.2.0";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/c0/2f/6773347277d76c5ade4414a6c3f785ef27e7f5c4b0870ec7e888e66a8d83/more-itertools-4.2.0.tar.gz"; sha256 = "2b6b9893337bfd9166bee6a62c2b0c9fe7735dcf85948b387ec8cba30e85d8e8"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [
      self."six"
    ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/erikrose/more-itertools";
        license = licenses.mit;
        description = "More routines for operating on iterables, beyond itertools";
      };
    };



    "oauthlib" = python.mkDerivation {
      name = "oauthlib-2.1.0";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/df/5f/3f4aae7b28db87ddef18afed3b71921e531ca288dc604eb981e9ec9f8853/oauthlib-2.1.0.tar.gz"; sha256 = "ac35665a61c1685c56336bda97d5eefa246f1202618a1d6f34fccb1bdd404162"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [
      self."cryptography"
    ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/oauthlib/oauthlib";
        license = licenses.bsdOriginal;
        description = "A generic, spec-compliant, thorough implementation of the OAuth request-signing logic";
      };
    };



    "pluggy" = python.mkDerivation {
      name = "pluggy-0.6.0";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/11/bf/cbeb8cdfaffa9f2ea154a30ae31a9d04a1209312e2919138b4171a1f8199/pluggy-0.6.0.tar.gz"; sha256 = "7f8ae7f5bdf75671a718d2daf0a64b7885f74510bcd98b1a0bb420eb9a9d0cff"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/pytest-dev/pluggy";
        license = licenses.mit;
        description = "plugin and hook calling mechanisms for python";
      };
    };



    "progressbar" = python.mkDerivation {
      name = "progressbar-2.5";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/a3/a6/b8e451f6cff1c99b4747a2f7235aa904d2d49e8e1464e0b798272aa84358/progressbar-2.5.tar.gz"; sha256 = "5d81cb529da2e223b53962afd6c8ca0f05c6670e40309a7219eacc36af9b6c63"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "http://code.google.com/p/python-progressbar";
        license = licenses.lgpl2;
        description = "Text progress bar library for Python.";
      };
    };



    "py" = python.mkDerivation {
      name = "py-1.5.4";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/35/77/a0a2a4126cf454e6ac772942898379e2fe78f2b7885df0461a5b8f8a8040/py-1.5.4.tar.gz"; sha256 = "3fd59af7435864e1a243790d322d763925431213b6b8529c6ca71081ace3bbf7"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "http://py.readthedocs.io/";
        license = licenses.mit;
        description = "library with cross-python path, ini-parsing, io, code, log facilities";
      };
    };



    "pyasn1" = python.mkDerivation {
      name = "pyasn1-0.4.4";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/10/46/059775dc8e50f722d205452bced4b3cc965d27e8c3389156acd3b1123ae3/pyasn1-0.4.4.tar.gz"; sha256 = "f58f2a3d12fd754aa123e9fa74fb7345333000a035f3921dbdaa08597aa53137"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/etingof/pyasn1";
        license = licenses.bsdOriginal;
        description = "ASN.1 types and codecs";
      };
    };



    "pyasn1-modules" = python.mkDerivation {
      name = "pyasn1-modules-0.2.2";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/37/33/74ebdc52be534e683dc91faf263931bc00ae05c6073909fde53999088541/pyasn1-modules-0.2.2.tar.gz"; sha256 = "a0cf3e1842e7c60fde97cb22d275eb6f9524f5c5250489e292529de841417547"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [
      self."pyasn1"
    ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/etingof/pyasn1-modules";
        license = licenses.bsdOriginal;
        description = "A collection of ASN.1-based protocols modules.";
      };
    };



    "pycparser" = python.mkDerivation {
      name = "pycparser-2.18";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/8c/2d/aad7f16146f4197a11f8e91fb81df177adcc2073d36a17b1491fd09df6ed/pycparser-2.18.tar.gz"; sha256 = "99a8ca03e29851d96616ad0404b4aad7d9ee16f25c9f9708a11faf2810f7b226"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/eliben/pycparser";
        license = licenses.bsdOriginal;
        description = "C parser in Python";
      };
    };



    "pytest" = python.mkDerivation {
      name = "pytest-3.6.4";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/05/b5/3197f0ae65c0b3037acd64c216a6d67a94693dad957f4e83e777b36688ae/pytest-3.6.4.tar.gz"; sha256 = "341ec10361b64a24accaec3c7ba5f7d5ee1ca4cebea30f76fad3dd12db9f0541"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [
      self."atomicwrites"
      self."attrs"
      self."colorama"
      self."funcsigs"
      self."more-itertools"
      self."pluggy"
      self."py"
      self."six"
    ];
      meta = with pkgs.stdenv.lib; {
        homepage = "http://pytest.org";
        license = licenses.mit;
        description = "pytest: simple powerful testing with Python";
      };
    };



    "python-dateutil" = python.mkDerivation {
      name = "python-dateutil-2.7.3";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/a0/b0/a4e3241d2dee665fea11baec21389aec6886655cd4db7647ddf96c3fad15/python-dateutil-2.7.3.tar.gz"; sha256 = "e27001de32f627c22380a688bcc43ce83504a7bc5da472209b4c70f02829f0b8"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [
      self."six"
    ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://dateutil.readthedocs.io";
        license = licenses.bsdOriginal;
        description = "Extensions to the standard Python datetime module";
      };
    };



    "redis" = python.mkDerivation {
      name = "redis-2.10.5";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/68/44/5efe9e98ad83ef5b742ce62a15bea609ed5a0d1caf35b79257ddb324031a/redis-2.10.5.tar.gz"; sha256 = "5dfbae6acfc54edf0a7a415b99e0b21c0a3c27a7f787b292eea727b1facc5533"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "http://github.com/andymccurdy/redis-py";
        license = licenses.mit;
        description = "Python client for Redis key-value store";
      };
    };



    "requests" = python.mkDerivation {
      name = "requests-2.14.2";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/72/46/4abc3f5aaf7bf16a52206bb0c68677a26c216c1e6625c78c5aef695b5359/requests-2.14.2.tar.gz"; sha256 = "a274abba399a23e8713ffd2b5706535ae280ebe2b8069ee6a941cb089440d153"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [
      self."cryptography"
      self."idna"
    ];
      meta = with pkgs.stdenv.lib; {
        homepage = "http://python-requests.org";
        license = licenses.asl20;
        description = "Python HTTP for Humans.";
      };
    };



    "requests-oauthlib" = python.mkDerivation {
      name = "requests-oauthlib-1.0.0";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/95/be/072464f05b70e4142cb37151e215a2037b08b1400f8a56f2538b76ca6205/requests-oauthlib-1.0.0.tar.gz"; sha256 = "8886bfec5ad7afb391ed5443b1f697c6f4ae98d0e5620839d8b4499c032ada3f"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [
      self."oauthlib"
      self."requests"
    ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/requests/requests-oauthlib";
        license = licenses.bsdOriginal;
        description = "OAuthlib authentication support for Requests.";
      };
    };



    "retrying" = python.mkDerivation {
      name = "retrying-1.3.3";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/44/ef/beae4b4ef80902f22e3af073397f079c96969c69b2c7d52a57ea9ae61c9d/retrying-1.3.3.tar.gz"; sha256 = "08c039560a6da2fe4f2c426d0766e284d3b736e355f8dd24b37367b0bb41973b"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [
      self."six"
    ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/rholder/retrying";
        license = licenses.asl20;
        description = "Retrying";
      };
    };



    "rsa" = python.mkDerivation {
      name = "rsa-3.4.2";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/14/89/adf8b72371e37f3ca69c6cb8ab6319d009c4a24b04a31399e5bd77d9bb57/rsa-3.4.2.tar.gz"; sha256 = "25df4e10c263fb88b5ace923dd84bf9aa7f5019687b5e55382ffcdb8bede9db5"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [
      self."pyasn1"
    ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://stuvel.eu/rsa";
        license = "License :: OSI Approved :: Apache Software License";
        description = "Pure-Python RSA implementation";
      };
    };



    "s3transfer" = python.mkDerivation {
      name = "s3transfer-0.1.13";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/9a/66/c6a5ae4dbbaf253bd662921b805e4972451a6d214d0dc9fb3300cb642320/s3transfer-0.1.13.tar.gz"; sha256 = "90dc18e028989c609146e241ea153250be451e05ecc0c2832565231dacdf59c1"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [
      self."botocore"
      self."futures"
    ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/boto/s3transfer";
        license = licenses.asl20;
        description = "An Amazon S3 Transfer Manager";
      };
    };



    "semantic-version" = python.mkDerivation {
      name = "semantic-version-2.6.0";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/72/83/f76958017f3094b072d8e3a72d25c3ed65f754cc607fdb6a7b33d84ab1d5/semantic_version-2.6.0.tar.gz"; sha256 = "2a4328680073e9b243667b201119772aefc5fc63ae32398d6afafff07c4f54c0"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/rbarrois/python-semanticversion";
        license = licenses.bsdOriginal;
        description = "A library implementing the 'SemVer' scheme.";
      };
    };



    "six" = python.mkDerivation {
      name = "six-1.11.0";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/16/d8/bc6316cf98419719bd59c91742194c111b6f2e85abac88e496adefaf7afe/six-1.11.0.tar.gz"; sha256 = "70e8a77beed4562e7f14fe23a786b54f6296e34344c23bc42f07b15018ff98e9"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "http://pypi.python.org/pypi/six/";
        license = licenses.mit;
        description = "Python 2 and 3 compatibility utilities";
      };
    };



    "urllib3" = python.mkDerivation {
      name = "urllib3-1.20";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/20/56/a6aa403b0998f857b474a538343ee483f5c02491bd1aebf61d42a3f60f77/urllib3-1.20.tar.gz"; sha256 = "97ef2b6e2878d84c0126b9f4e608e37a951ca7848e4855a7f7f4437d5c34a72f"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [
      self."certifi"
      self."cryptography"
      self."idna"
      self."ipaddress"
    ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://urllib3.readthedocs.io/";
        license = licenses.mit;
        description = "HTTP library with thread-safe connection pooling, file post, and more.";
      };
    };



    "webencodings" = python.mkDerivation {
      name = "webencodings-0.5.1";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/0b/02/ae6ceac1baeda530866a85075641cec12989bd8d31af6d5ab4a3e8c92f47/webencodings-0.5.1.tar.gz"; sha256 = "b36a1c245f2d304965eb4e0a82848379241dc04b865afcc4aab16748587e1923"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/SimonSapin/python-webencodings";
        license = licenses.bsdOriginal;
        description = "Character encoding aliases for legacy web content";
      };
    };



    "websocket-client" = python.mkDerivation {
      name = "websocket-client-0.48.0";
      src = pkgs.fetchurl { url = "https://files.pythonhosted.org/packages/28/85/df04ec21c622728316b591c2852fd20a0e74324eeb6ca26f351844ba815f/websocket_client-0.48.0.tar.gz"; sha256 = "18f1170e6a1b5463986739d9fd45c4308b0d025c1b2f9b88788d8f69e8a5eb4a"; };
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs;
      propagatedBuildInputs = [
      self."six"
    ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/websocket-client/websocket-client.git";
        license = licenses.lgpl2;
        description = "WebSocket client for python. hybi13 is supported.";
      };
    };

  };
  localOverridesFile = ./requirements_override.nix;
  overrides = import localOverridesFile { inherit pkgs python; };
  commonOverrides = [

  ];
  allOverrides =
    (if (builtins.pathExists localOverridesFile)
     then [overrides] else [] ) ++ commonOverrides;

in python.withPackages
   (fix' (pkgs.lib.fold
            extends
            generated
            allOverrides
         )
   )