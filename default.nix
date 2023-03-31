{ci ? false, haskellCompiler ? "ghc927" }:
let
  # Import the Haskell.nix library,
  haskell-src = import ((import ./nix/sources.nix)."haskell.nix") {};
  npSrc = haskell-src.sources.nixpkgs-unstable;
  npArgs = haskell-src.nixpkgsArgs;
  pin = import npSrc npArgs;

  haskell = pin.haskell-nix;

  ciOptions = [ { packages.eventlog2html.configureFlags = [ "--ghc-option=-Werror" ]; } ];

  opts = [ { packages.vault.doHaddock = false; } ];

  # Instantiate a package set using the generated file.
  pkgSet = haskell.cabalProject {
    compiler-nix-name = haskellCompiler;
    src = haskell.haskellLib.cleanGit { name = "eventlog2html"; src = ./.; };
    modules = (if ci then ciOptions else []) ++ opts;
    index-state = "2023-03-25T00:00:00Z";
  };


  site = import ./nix/site.nix { nixpkgs = pin; hspkgs = pkgSet; };

in
  { eventlog2html = pkgSet.eventlog2html.components.exes.eventlog2html ;
  site = site;
  }
