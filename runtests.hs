module Main where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Language.Syntactical.Yard
import Language.Syntactical.Tests.Examples

main :: IO ()
main = defaultMain
  [ testYard
  ]

testYard :: Test
testYard = testGroup "Language.Syntactical.Yard"
  [ testGroup "Language.Syntactical.Tests.Examples - table0" $
    map (helper table0) testsTable0
  ]

helper table (i,o) = testCase i $ case parse table i of
  S [] [] [[o']] Success -> o @?= show o'
  _ -> assertFailure "cannot parse"

