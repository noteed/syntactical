{-# Language FlexibleInstances #-}
{-# Language TypeSynonymInstances #-}
module Main where

import Data.List (head)
import Data.String
import Protolude hiding (head, words, Associativity, First, Infix, LeftAssociative, Prefix, Last, RightAssociative)
import Data.List (intersperse)
import System.Environment (getArgs)
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Text.Syntactical
import Text.Syntactical.Data

import qualified Ambiguous
import qualified Holes
import qualified Indent
import qualified Priority
import qualified Simple

-- Make it possible to shunt around some strings.
instance Token String where
  toString = identity
  operator o as = List $ Atom a : as
    where a = case o of
            Op1 _ _ _ (Infix _) _ -> '␣' : a' ++ "␣"
            Op1 _ _ _ Prefix _ -> a' ++ "␣"
            Op1 _ _ _ Postfix _ -> '␣' : a'
            Op2 _ _ _ _ _ -> a'
          a' = concat . intersperse "␣" $ symbols o

tokenize = map token . separate

separate = words . separate'
separate' ('(':cs) = " ( " ++ separate' cs
separate' (')':cs) = " ) " ++ separate' cs
separate' ('⟨':cs) = " ⟨ " ++ separate' cs
separate' ('⟩':cs) = " ⟩ " ++ separate' cs
separate' (c:cs) = c : separate' cs
separate' [] = []

token = Atom

parseSimple = shunt Simple.table . tokenize

parseHoles = shunt Holes.table . tokenize

parsePriority = shunt Priority.table . tokenize

parseAmbiguous = shunt Ambiguous.table . tokenize

stepsSimple = steps Simple.table . tokenize

stepsHoles = steps Holes.table . tokenize

stepsPriority = steps Priority.table . tokenize

stepsAmbiguous = steps Ambiguous.table . tokenize

-- 

main :: IO ()
main = defaultMain
  [ testYard
  ]

testYard :: Test
testYard = testGroup "Text.Syntactical.Yard"
  [ testGroup "Simple" $
    map (helper parseSimple) Simple.tests
  , testGroup "Simple (bad input)" $
    map (helper' parseSimple) Simple.tests'
  , testGroup "Holes" $
    map (helper parseHoles) Holes.tests
  , testGroup "Priority (associativity and precedence)" $
    map (helper parsePriority) Priority.tests
  , testGroup "Priority (associativity and precedence) (bad input)" $
    map (helper' parsePriority) Priority.tests'
  , testGroup "Ambiguous" $
    map (helper' parseAmbiguous) Ambiguous.tests
  , Indent.tests
  ]

-- Apply the parser p to i and check if it returns
-- the expected value o.
helper p (i,o) = testCase i $ case p i of
  Right o' -> o @=? showSExpr o'
  Left err -> assertFailure $ "cannot parse: " ++ show err

helper' p (i,o) = testCase i $ case p i of
  Right o' -> assertFailure $ "unexpected successful parse: " ++ showSExpr o'
  Left o' -> o @=? o'
