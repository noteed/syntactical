-- | This convenience module re-exports from 'Text.Syntactical.Yard'
-- and 'Text.Syntactical.Data' everything a typical usage of Syntactical
-- would need.

module Text.Syntactical (
  -- * Parsing
  shunt, steps, Failure(..), Ambiguity(..), showFailure,
  -- * Operators
  Op, Associativity(..), Hole(..),
  infx, prefx, postfx, closed,
  infx_, prefx_, postfx_, closed_,
  sexpr, distfix,
  arity, symbol, symbols, next, previous, current,
  -- * Operator tables
  Table, buildTable,
  -- * Tokens
  Token(..),
  -- * S-Expressions
  SExpr(..), showSExpr
  ) where

import Text.Syntactical.Yard
  (shunt, steps, Failure(..), showFailure)
import Text.Syntactical.Data (
  infx, prefx, postfx, closed,
  infx_, prefx_, postfx_, closed_,
  sexpr, distfix,
  buildTable,
  arity, symbol, symbols, next, previous, current,
  Associativity(..), Hole(..), Table, Op, SExpr(..), Ambiguity(..),
  Token(..),
  showSExpr
  )
