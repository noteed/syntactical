-- Convenience module exporting what's important.
module Text.Syntactical (
  module Text.Syntactical.Yard,
  module Text.Syntactical.Data
  ) where

import Text.Syntactical.Yard
  (shunt, steps, Failure(..), showFailure)
import Text.Syntactical.Data (
  infx, prefx, postfx, closed,
  infx_, prefx_, postfx_, closed_,
  sexpr, distfix,
  buildTable,
  arity, symbol, symbols, next, previous, current,
  Associativity(..), Hole(..), Table, SExpr(..), Ambiguity(..),
  Token, toString, operator, consider,
  showSExpr
  )
