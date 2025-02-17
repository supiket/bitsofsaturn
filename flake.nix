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

          buildInputs = with pkgs; [ haskellPackages.cabal2nix gcc libgit2 zlib ];

          meta = with pkgs.lib; {
            description = "My Haskell Project";
            license = licenses.mit;
          };
        };

        devShells.${system}.default = pkgs.mkShell {
          buildInputs = with pkgs; [ haskellPackages.cabal2nix pkgs.gcc pkgs.zlib ];
        };

        packages.${system}.my-haskell-app = pkgs.stdenv.mkDerivation rec {
          pname = "bitsofsaturn";
          version = "0.1.0.0";
          src = ./.;

          buildInputs = with pkgs; [ haskellPackages.cabal2nix gcc libgit2 zlib ];
        };
      });
}
