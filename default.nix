let
  pin = import ((import ./nix/sources.nix).nixpkgs) {} ;
  # Import the Haskell.nix library,
  haskell = import ((import ./nix/sources.nix)."haskell.nix") { pkgs = pin; };

  pkgPlan = haskell.importAndFilterProject (haskell.callCabalProjectToNix
              { index-state = "2019-09-05T00:00:00Z"
              ; src = pin.lib.cleanSource ./.;});

  # Instantiate a package set using the generated file.
  pkgSet = haskell.mkCabalProjectPkgSet {
    plan-pkgs = pkgPlan.pkgs;
    pkg-def-extras = [];
    modules = [];
  };


  site = import ./nix/site.nix { nixpkgs = pin; hspkgs = pkgSet.config.hsPkgs; };

in
  { eventlog2html = pkgSet.config.hsPkgs.eventlog2html.components.exes.eventlog2html ;
    site = site; }
