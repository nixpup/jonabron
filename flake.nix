{
  description = "G(N)eex: a custom Nix Channel/Input for various Packages";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = inputs@{ self, nixpkgs, ... }:
  let
    pkgs = nixpkgs.legacyPackages.x86_64-linux;
    gobm = import ./nix/packages/gobm/default.nix { inherit pkgs; };
    #otherpkg = import ./nix/packages/otherpkg/default.nix { inherit pkgs; };
  in
  {
    packages.x86_64-linux = {
      gobm = gobm;
      #otherpkg = otherpkg;
    };
    apps.x86_64-linux = {
      gobm = {
        type = "app";
        program = "${gobm}/bin/gobm";
        meta = {
          description = "Download osu! Beatmaps from a URL";
          mainProgram = "gobm";
        };
      };
      #otherpkg = { ... };
    };
    overlays.default = final: prev: {
      gobm = gobm;
    };
  };
}
