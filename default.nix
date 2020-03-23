{ci ? false, haskellCompiler ? "ghc881" }:
let
  # Import the Haskell.nix library,
  haskell-src = import ((import ./nix/sources.nix)."haskell.nix");
  pin = import ((import ./nix/sources.nix).nixpkgs) haskell-src ;
#  pin = import <nixpkgs> haskell-src ;

  haskell = pin.haskell-nix;

  ciOptions = [ { packages.eventlog2html.configureFlags = [ "--ghc-option=-Werror" ]; } ];

  opts = [ { packages.vault.doHaddock = false; } ];

  # Instantiate a package set using the generated file.
  pkgSet = haskell.cabalProject {
    src = haskell.haskellLib.cleanGit { src = ./.; };
    ghc = pin.buildPackages.pkgs.haskell-nix.compiler.${haskellCompiler};
    modules = (if ci then ciOptions else []) ++ opts;
  };


  site = import ./nix/site.nix { nixpkgs = pin; hspkgs = pkgSet; };

in
  { eventlog2html = pkgSet.eventlog2html.components.exes.eventlog2html ;
  site = site;
  }
