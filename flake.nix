{
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system}; in
      {
        packages = rec {
          bench = pkgs.haskellPackages.callCabal2nix "popcount-benchmark" ./. {};
          default = bench;
        };
        apps = rec {
          bench = flake-utils.lib.mkApp { drv = self.packages.${system}.bench; exePath = "/bin/benchmark"; };
          default = bench;
        };
      }
    );
}
