with { pkgs = import (import ./nix/sources.nix).nixpkgs {}; };
pkgs.mkShell
  { buildInputs = [ pkgs.haskell.compiler.ghc865 pkgs.zlib pkgs.haskellPackages.cabal-install ];
    shellHook=''export LD_LIBRARY_PATH=${pkgs.gmp}/lib:${pkgs.zlib}/lib:${pkgs.ncurses}/lib'';
  }
