{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          name = "git-pair";

          pkgs = nixpkgs.legacyPackages.${system};

          inherit (nixpkgs.lib) composeManyExtensions;
          inherit (pkgs.haskell.lib) packageSourceOverrides justStaticExecutables;
          inherit (flake-utils.lib) mkApp;

          hspkgs = pkgs.haskellPackages.extend (composeManyExtensions [
            (self: super: {
              ${name} = self.callCabal2nix name ./. {};
            })
            # (packageSourceOverrides {
            #   ${name} = ./.;
            # })
          ]);
        in
        {
          # inherit pkgs hspkgs; # convenient for debugging sometimes

          packages = {
            ${name} = justStaticExecutables hspkgs.${name};
            default = self.packages.${system}.${name};
          };

          apps = {
            ${name} = mkApp { drv = self.packages.${system}.${name}; };
            default = self.apps.${system}.${name};
          };

          devShells.default = hspkgs.shellFor {
            packages = p: [
              p.${name}
            ];
            # exactDeps = false; # work around https://github.com/input-output-hk/haskell.nix/issues/839
            buildInputs = [
              pkgs.cabal-install

              hspkgs.haskell-language-server
              hspkgs.ghcid

              hspkgs.hlint

              hspkgs.ormolu
              pkgs.nixpkgs-fmt
              hspkgs.cabal-fmt
            ];
          };
        });
}
