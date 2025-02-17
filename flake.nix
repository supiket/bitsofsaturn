
{
  description = "bits of saturn";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachSystem ["x86_64-linux" "aarch64-linux" "aarch64-darwin"] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        defaultPackage = pkgs.stdenv.mkDerivation rec {
          pname = "bitsofsaturn";
          version = "0.1.0.0";
          src = ./.;

          buildInputs = with pkgs; [
            haskellPackages.cabal2nix
            gcc
            gmp
            libgit2
            (if system == "aarch64-darwin" then pkgs.zlib else null)
          ];
        };

        devShells.${system}.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.cabal2nix
            gcc
            gmp
            (if system == "aarch64-darwin" then pkgs.zlib else null)
          ];
        };
      });
}
