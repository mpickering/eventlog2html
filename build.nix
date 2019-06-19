let
  pin = import ((import ./nix/sources.nix).nixpkgs) {} ;
  # Import the Haskell.nix library,
  haskell = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) { pkgs = pin; };

  pkgPlan = haskell.callCabalProjectToNix {index-state = "2019-05-10T00:00:00Z"; src = ./.;};

  # Instantiate a package set using the generated file.
  pkgSet = haskell.mkCabalProjectPkgSet {
    plan-pkgs = import pkgPlan;
    pkg-def-extras = [];
    modules = [];
  };


  site = import ./nix/site.nix { nixpkgs = pin; hspkgs = pkgSet.config.hsPkgs; };

in
  { eventlog2html = pkgSet.config.hsPkgs.eventlog2html.components.exes.eventlog2html ;
    site = site; }
