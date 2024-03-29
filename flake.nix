{
  description = "launch";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        app = pkgs.stdenv.mkDerivation rec {
          name = "launch-${version}";
          version = "0.1.0";
          depsBuildBuild = [ pkgs.nim ];
          buildInputs = [ pkgs.pcre pkgs.wtype pkgs.nix-index ];
          src = ./src;
          buildPhase = ''
            export WTYPE_BIN="${pkgs.wtype}/bin/wtype"
            export NIX_LOCATE_BIN="${pkgs.nix-index}/bin/nix-locate"
            TMP=$(realpath .)
            nim compile \
              -d:release \
              --threads:on \
              --nimcache:$TMP \
              --out:launch \
              ${src}/main.nim
          '';
          installPhase = ''
            install -Dt \
              $out/bin \
              launch
          '';

          NIX_LDFLAGS = "-lpcre";

          meta = with pkgs.lib; {
            description = "launcher for personal use";
            homepage = "https://github.com/jwoudenberg/launch";
            license = licenses.mit;
          };
        };

      in {
        defaultPackage = app;
        devShell = pkgs.mkShell {
          buildInputs = [ pkgs.nim ];
          WTYPE_BIN = "${pkgs.wtype}/bin/wtype";
          NIX_LOCATE_BIN = "${pkgs.nix-index}/bin/nix-locate";
        };
      });
}
