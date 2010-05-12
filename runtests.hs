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
    map (helper parse0) testsTable0
  ]

-- Apply the parser p to i and check if it returns
-- the expected value o.
helper p (i,o) = testCase i $ case p i of
  Right o' -> o @?= show o'
  _ -> assertFailure "cannot parse"

