-- Convenience module exporting what's important.
module Text.Syntactical
  ( shunt, steps
  , Associativity(..), Kind(..), Op(..), Table(..), Tree(..)
  ) where

import Text.Syntactical.Yard (shunt, steps)
import Text.Syntactical.Data
  (Associativity(..), Kind(..), Op(..), Table(..), Tree(..))
