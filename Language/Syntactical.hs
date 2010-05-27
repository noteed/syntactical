-- Convenience module exporting what's important.
module Language.Syntactical
  ( shunt, steps
  , Associativity(..), Kind(..), Op(..), Table(..), Tree(..)
  ) where

import Language.Syntactical.Yard (shunt, steps)
import Language.Syntactical.Data
  (Associativity(..), Kind(..), Op(..), Table(..), Tree(..))
