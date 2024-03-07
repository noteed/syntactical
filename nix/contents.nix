let

  sources = import ./sources.nix;
  defNixpkgs = import sources.nixpkgs { };
  nix-filter = import sources.nix-filter;

in { nixpkgs ? defNixpkgs }:

let
  inherit (nixpkgs.lib.attrsets) getAttrFromPath mapAttrs;

  # Lists all packages made available through this nix project.
  # The format is `{ <pkgName> : <pkgDir> }` (we refer to this as pInfo).
  # The used directory should be the path of the directory relative to the root
  # of the project.
  pkgList = {
    syntactical = nix-filter {
      root = ../.;
      include = with nix-filter; [
        "syntactical.cabal"
        "LICENSE"
        (and "bin" (or_ (matchExt "hs") isDirectory))
        (and "src" (or_ (matchExt "hs") isDirectory))
        (and "tests" (or_ (matchExt "hs") isDirectory))
      ];
    };
  };
in {
  inherit pkgList;

  # Get an attribute from a string path from a larger attrSet
  getPkg = pkgs: pPath: getAttrFromPath [pPath] pkgs;

  # The overriding function that can provide our package to e.g. an overlay.
  overrides = selfh: superh:
    let
      callCabalOn = name: dir:
        selfh.callCabal2nix "${name}" dir { };

    in mapAttrs callCabalOn pkgList;
}
