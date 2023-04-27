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
          inherit pkgs hspkgs;
          packages = {
            ${name} = justStaticExecutables hspkgs.${name};
            default = self.packages.${system}.${name};
          };

          devShells.default = hspkgs.shellFor {
            packages = p: [
              p.${name}
            ];
            extraDependencies = p: { executableHaskellDepends = [ p.relude ]; };
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
