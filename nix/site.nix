{ nixpkgs, hspkgs }:
nixpkgs.stdenv.mkDerivation {
  name = "docs-0.1";

  src = nixpkgs.lib.cleanSource ../docs;
  LANG = "en_US.UTF-8";
  LOCALE_ARCHIVE = "${nixpkgs.glibcLocales}/lib/locale/locale-archive";

  buildInputs = [ hspkgs.hakyll-eventlog.components.exes.site ];

  preConfigure = ''
    export LANG="en_US.UTF-8";
    '';

  buildPhase = ''
    site build
  '';

  installPhase = ''
    cp -r _site $out
  '';
}


