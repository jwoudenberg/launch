{
  description = "A terminal-based launcher for personal use";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    { nixpkgs, ... }:
    let
      pkgs = nixpkgs.legacyPackages."x86_64-linux";
    in
    {
      devShell."x86_64-linux" = pkgs.mkShell {
        packages = [ pkgs.zig ];
      };
    };
}
