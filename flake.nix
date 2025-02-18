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
            cabal-install
            gcc-unwrapped
            ghc
            glibcLocales
            gmp
            haskellPackages.cabal2nix
            libgit2
          ];

          shellHook = ''
            export LANG=en_US.UTF-8
            export LC_ALL=en_US.UTF-8
          '';
        };

        devShells.${system}.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            cabal-install
            gcc-unwrapped
            ghc
            glibcLocales
            gmp
            haskellPackages.cabal2nix
          ];

          shellHook = ''
            export LANG=en_US.UTF-8
            export LC_ALL=en_US.UTF-8
          '';
        };
      });
}
