-- Convenience module exporting what's important.
module Text.Syntactical (
  shunt, Failure(..),
  infx, prefx, postfx, closed,
  infx_, prefx_, postfx_, closed_,
  sexpr, distfix,
  buildTable,
  Associativity(..), Kind(..), Table, SExpr(..),
  Token, string, operator, consider
  ) where

import Text.Syntactical.Yard
  (shunt, Failure(..))
import Text.Syntactical.Data (
  infx, prefx, postfx, closed,
  infx_, prefx_, postfx_, closed_,
  sexpr, distfix,
  buildTable,
  Associativity(..), Kind(..), Table, SExpr(..),
  Token, string, operator, consider
  )
