#! /usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix

runghc -isrc/ -itests/ tests/permutations.hs
