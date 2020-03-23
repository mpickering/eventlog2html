with { pkgs = import (import ./nix/sources.nix).nixpkgs {}; };
pkgs.mkShell
  { buildInputs = [ pkgs.haskell.compiler.ghc881 pkgs.zlib pkgs.haskellPackages.cabal-install pkgs.git-lfs ];
    shellHook=''export LD_LIBRARY_PATH=${pkgs.gmp}/lib:${pkgs.zlib}/lib:${pkgs.ncurses}/lib'';
  }
