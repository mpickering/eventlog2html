with { pkgs = import (import ./nix/sources.nix).nixpkgs {}; };
pkgs.mkShell
  { buildInputs = [ pkgs.haskell.compiler.ghc865 pkgs.haskellPackages.cabal-install ];
  }
