-- Convenience module exporting what's important.
module Text.Syntactical
  ( shunt, Failure(..)
  , infx, prefx, postfx, closed, buildTable
  , Associativity(..), Kind(..), Table, Tree(..)
  ) where

import Text.Syntactical.Yard
  (shunt, Failure(..))
import Text.Syntactical.Data
  (infx, prefx, postfx, closed, buildTable
  , Associativity(..), Kind(..), Table, Tree(..))
