let
  sources = import ./nix/sources.nix;
  overlays = import ./nix/overlays.nix;
  nixpkgs = import sources.nixpkgs { inherit overlays; };

in rec
  {
    # Build with nix-build -A <attr>
    binaries = nixpkgs.haskellPackages.syntactical;
    # binaries + haddock are also available as binaries.all.
    haddock = nixpkgs.haskellPackages.syntactical.doc;

    # A shell to try out our binaries
    # Run with nix-shell default.nix -A shell
    shell = nixpkgs.mkShell {
      buildInputs = [
        binaries
      ];
      shellHook = ''
        # source <(syntactical-indent --bash-completion-script `which syntactical-indent`)
      '';
    };
  }
