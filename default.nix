let
  reflex = import ./reflex-platform {};
  # Hide the 'dist' directory. Having cabal configured is great for incremental development
  hideDist = src: builtins.filterSource (path: type: type != "unknown" && baseNameOf path != "dist") src;
  # Put our packages into the GHC
  ghc = reflex.ghc.override {
    overrides = self: super: {
      reflex-websockets = self.callPackage (reflex.cabal2nixResult (hideDist ./reflex-websockets)) {};
      example = self.callPackage (reflex.cabal2nixResult (hideDist ./example)) {};
    };
  };
in ghc.example
