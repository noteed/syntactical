{-# Language FlexibleInstances #-}
{-# Language TypeSynonymInstances #-}
-- A test where tokens are given in random order to see if the
-- algorithm terminates always in a valid state (i.e. not Unexpected).
module Main where

import Data.List (permutations)
import Data.String
import Protolude hiding (head, words, Associativity, First, Infix, LeftAssociative, Prefix, Last, RightAssociative)
import System.Environment (getArgs)
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Text.Syntactical
import Text.Syntactical.Yard
import Text.Syntactical.Data

import qualified Holes

-- Make it possible to shunt around some strings.
instance Token String where
  toString = identity
  operator o as = List $
    (Atom . concat $ symbols o) : as

table0 :: Table String
table0 = buildTable
 [ [ closed_ "(" Distfix ")"
   , closed_ "⟨" SExpression "⟩"
   , closed "</" Distfix"/>"
   , closed "[" Distfix "|" `distfix` "]"
   , closed "{" Distfix "}"
   ]
 , [ infx RightAssociative "?'" `distfix` ":'"
   , postfx "!"
   , postfx "_/" `distfix` "/."
   ]
 , [ postfx "%"
   , prefx "#"
   ]
 , [ postfx "°"
   , infx LeftAssociative "*"
   , infx LeftAssociative "/"
   ]
 , [ infx LeftAssociative "+"
   , infx LeftAssociative "-"
   ]
 , [ infx LeftAssociative "<<"
   , infx LeftAssociative ">>"
   , infx RightAssociative "?" `distfix` ":"
   ]
 , [ prefx "if" `distfix` "then" `distfix` "else"
   ]
 , [ infx RightAssociative ","
   ]
 , [ Op1 True "\\" [(SExpression,"->")] Prefix 0
   ]
 , [ prefx "let" `distfix` "in"
   , infx RightAssociative "where"
   , prefx "case" `distfix` "of"
   ]
 , [ infx NonAssociative "="
   ]
 , [ infx RightAssociative ";"
   ]
 , [ infx RightAssociative "::" `distfix` ";;"
   , infx RightAssociative "==" `distfix` ";;"
   ]
 ]

parse0 = shunt table0 . tokenize

steps0 = steps table0 . tokenize

tokenize = map Atom

inputs1 = permutations . words $
 "* / a b c"

inputs2 = permutations . words $
 "* / + a b c d"

inputs3 = permutations . words $
 "( ) a b c"

-- No hope for this one (too many permutations).
inputsZ = permutations . words $
 "( ) ⟨ ⟩ </ >/ [ ] | { } ? : _/ /. % # ° * / + - << >> " ++
 "if then else , \\ -> let in where case of = ; :: ;; == ;; " ++
 "a b c d e f g h i j k l m n o p q r s t u v w x y z " ++
 "1 2 3 4 5 6 7 8 9 0 11 12 13 14 15 16 17 18 19 20 21 22 23 24"

--

main :: IO ()
main = defaultMain
  [ testYard
  ]

testYard :: Test
testYard = testGroup "Text.Syntactical.Yard"
  [ testGroup "Permutations 1" $
    map (valid parse0) inputs1
  , testGroup "Permutations 2" $
    map (valid parse0) inputs2
  , testGroup "Permutations 3" $
    map (valid parse0) inputs3
  ]

-- Apply the parser p to i and check if it returns a valid state.
valid p i = testCase (concat i) $ case p i of
  Left Unexpected -> assertFailure "Failure Unexpected possible"
  _ -> return ()
