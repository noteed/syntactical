-- Convenience module exporting what's important.
module Text.Syntactical
  ( shunt, steps, Result(..), Failure(..), showFailure
  , infx, prefx, postfx, closed, buildTable
  , Associativity(..), Kind(..), Table, Tree(..)
  ) where

import Text.Syntactical.Yard
  (shunt, steps, Result(..), Failure(..), showFailure)
import Text.Syntactical.Data
  (infx, prefx, postfx, closed, buildTable
  , Associativity(..), Kind(..), Table, Tree(..))
