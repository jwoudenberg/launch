{
  description = "launch";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.09";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        launch = pkgs.haskellPackages.callCabal2nix "launch" ./. { };
      in {
        defaultPackage = pkgs.haskell.lib.justStaticExecutables launch;
        devShell =

          pkgs.haskellPackages.shellFor {
            packages = p: [ launch ];
            buildInputs = [
              pkgs.cabal-install
              pkgs.haskellPackages.ghcid
              pkgs.haskellPackages.hpack
              pkgs.ormolu
            ];
          };
      });
}
