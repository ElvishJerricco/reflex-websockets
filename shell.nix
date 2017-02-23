let reflex = import ./reflex-platform {};
in (reflex.nixpkgs.haskell.lib.addBuildDepends (import ./.) [reflex.ghc.ghcid reflex.ghc.cabal-install]).env
