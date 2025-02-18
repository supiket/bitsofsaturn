FROM nixos/nix

WORKDIR /app
COPY . .

RUN nix-env -iA nixpkgs.nix && \
    nix-channel --update && \
    nix develop --command bash -c "stack build"

CMD ["nix", "develop", "--command", "stack", "run"]
