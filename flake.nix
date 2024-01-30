# Docs for this file: https://github.com/input-output-hk/iogx/blob/main/doc/api.md#flakenix
{
  description = "Computer graphics libraries for Haskell.";


  inputs = {
    iogx = {
      url = "github:input-output-hk/iogx";
      inputs.hackage.follows = "hackage";
      inputs.haskell-nix.follows = "haskell-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.follows = "haskell-nix/nixpkgs";

    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };

    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.hackage.follows = "hackage";
    };
  };


  # Docs for mkFlake: https://github.com/input-output-hk/iogx/blob/main/doc/api.md#mkflake
  outputs = inputs: inputs.iogx.lib.mkFlake {

    inherit inputs;

    repoRoot = ./.;

    outputs = import ./nix/outputs.nix;

    systems = [ "x86_64-linux" ];

    # debug = false;

    # flake = {};
  };


  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = true;
  };
}
