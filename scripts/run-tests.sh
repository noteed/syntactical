#! /usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix

runghc -itests/ tests/suite.hs
