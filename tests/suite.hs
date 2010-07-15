{-# Language TypeSynonymInstances #-}
module Main where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import System.Environment (getArgs)

import Text.Syntactical
import Text.Syntactical.Data

import qualified Simple
import qualified Holes
import qualified Priority

-- Make it possible to shunt around some strings.
instance Token String where
  toString = id
  operator o as = List $
    (Atom . concat $ symbols o) : as

tokenize = map (token) . separate

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

stepsSimple = steps Simple.table . tokenize

stepsHoles = steps Holes.table . tokenize

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
  ]

-- Apply the parser p to i and check if it returns
-- the expected value o.
helper p (i,o) = testCase i $ case p i of
  Right o' -> o @=? showSExpr o'
  Left err -> assertFailure $ "cannot parse: " ++ show err

helper' p (i,o) = testCase i $ case p i of
  Right o' -> assertFailure $ "unexpected successful parse: " ++ showSExpr o'
  Left o' -> o @=? o'

