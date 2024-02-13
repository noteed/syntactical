#! /usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix

runghc \
  -isrc/ \
  -itests/ \
  -XNoImplicitPrelude \
  -XOverloadedStrings \
  -XTypeApplications \
  -Wall \
  tests/permutations.hs
