FROM nixos/nix

WORKDIR /app
COPY . .

RUN nix-env -iA nixpkgs.nix && \
    nix-channel --update && \
    nix develop --extra-experimental-features nix-command --extra-experimental-features flakes --command bash -c "stack build"

CMD ["nix", "develop", "--extra-experimental-features", "nix-command flakes", "--command", "stack", "run"]
