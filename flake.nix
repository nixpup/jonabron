{
  description = "G(N)eex: a custom Nix Channel/Input for various Packages";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = inputs@{ self, nixpkgs, ... }:
  let
    pkgs = nixpkgs.legacyPackages.x86_64-linux;
    gobm = import ./nix/packages/gobm/default.nix { inherit pkgs; };
    urbit = import ./nix/packages/urbit/default.nix { inherit pkgs; };
    #otherpkg = import ./nix/packages/otherpkg/default.nix { inherit pkgs; };
  in
  {
    packages.x86_64-linux = {
      gobm = gobm;
      urbit = urbit;
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
      urbit = {
        type = "app";
        program = "${urbit}/bin/urbit";
        meta = {
          description = "A clean-slate OS and network for the 21st century.";
          mainProgram = "urbit";
        };
      };
      #otherpkg = { ... };
    };
    overlays.default = final: prev: {
      gobm = gobm;
      urbit = urbit;
    };
  };
}
