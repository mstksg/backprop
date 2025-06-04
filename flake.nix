{
  description = "Basic Haskell Project Flake";
  inputs = {
    haskellProjectFlake.url = "github:mstksg/haskell-project-flake";
    nixpkgs.follows = "haskellProjectFlake/nixpkgs";
  };
  outputs =
    { self
    , nixpkgs
    , flake-utils
    , haskellProjectFlake
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      name = "backprop";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ haskellProjectFlake.overlays."${system}".default ];
      };
      project-flake = pkgs.haskell-project-flake
        {
          inherit name;
          src = ./.;
          # lapack seems to link badly on almost all versions so CI is
          # screwed until we figure out what's going on
          excludeCompilerMajors = [ "ghc94" "ghc913" ];
          defaultCompiler = "ghc982";
        };
    in
    {
      packages = project-flake.packages;
      apps = project-flake.apps;
      checks = project-flake.checks;
      devShells = project-flake.devShells;
      legacyPackages."${name}" = project-flake;
    }
    );
}

