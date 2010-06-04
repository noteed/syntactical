module Main where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Text.Syntactical.Yard
import Text.Syntactical.Tests.Examples

main :: IO ()
main = defaultMain
  [ testYard
  ]

testYard :: Test
testYard = testGroup "Text.Syntactical.Yard"
  [ testGroup "Text.Syntactical.Tests.Examples - table0" $
    map (helper parse0) testsTable0
  , testGroup "Text.Syntactical.Tests.Examples - table0 - bad input" $
    map (helper' parse0) testsTable0'
  ]

-- Apply the parser p to i and check if it returns
-- the expected value o.
helper p (i,o) = testCase i $ case p i of
  Right o' -> o @=? show o'
  Left err -> assertFailure $ "cannot parse: " ++ show err

helper' p (i,o) = testCase i $ case p i of
  Right o' -> assertFailure $ "unexpected successful parse: " ++ show o'
  Left (S _ _ _ (Done o')) -> o @=? o'

