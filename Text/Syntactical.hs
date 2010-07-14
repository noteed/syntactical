-- Convenience module exporting what's important.
module Text.Syntactical (
  module Text.Syntactical.Yard,
  module Text.Syntactical.Data
  ) where

import Text.Syntactical.Yard
  (shunt, steps, Failure(..))
import Text.Syntactical.Data (
  infx, prefx, postfx, closed,
  infx_, prefx_, postfx_, closed_,
  sexpr, distfix,
  buildTable,
  arity, partSymbol, nextPart, previousPart,
  Associativity(..), Kind(..), Table, SExpr(..),
  Token, toString, operator, consider
  )
