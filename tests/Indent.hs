module Indent where

import Data.Either (isLeft)
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Text.ParserCombinators.Parsec

import Text.Syntactical.Indent


--------------------------------------------------------------------------------
tests = testGroup "Indent"
  [ testCase "one atom"          $ tokenize "f"           @?= Right "f"
  , testCase "one atom"          $ tokenize " f"          @?= Right "f"

  , testCase "parse error" $ assertBool "" $ isLeft $ tokenize " f\na"

  , testCase "application"       $ tokenize "f a"         @?= Right "f a"
  , testCase "application"       $ tokenize "f a b"       @?= Right "f a b"
  , testCase "two atoms"         $ tokenize "f\na"        @?= Right "f ; a"
  , testCase "application"       $ tokenize "f\n  a"      @?= Right "f a"
  , testCase "application"       $ tokenize "f\n  a b"    @?= Right "f a b"
  , testCase "application"       $ tokenize "f\n  a\n  b" @?= Right "f a b"

    -- Even though b is less indented than a, it is still considered an argument
    -- of f. This is similar to what GHC does.
  , testCase "application"       $ tokenize "f\n  a\n b"  @?= Right "f a b"

  , testCase "application, atom" $ tokenize "f\n  a\nb"   @?= Right "f a ; b"
  , testCase "three atoms"       $ tokenize "f\na\nb"     @?= Right "f ; a ; b"

  , testCase "do"                $ tokenize "do f"        @?= Right "do { f }"
  , testCase "do"                $ tokenize "do f a"      @?= Right "do { f a }"
  , testCase "do"                $ tokenize "do f\na"     @?= Right "do { f } ; a"

    -- TODO The two following cases should make an error, although they can be
    -- rejected by the SExpr-consuming "parser" instead.
  , testCase "do"                $ tokenize "do f\n a"    @?= Right "do { f } a"
  , testCase "do"                $ tokenize "do f\n  a"   @?= Right "do { f } a"

  , testCase "do"                $ tokenize "do f\n   a"  @?= Right "do { f ; a }"
  , testCase "do"                $ tokenize "do f\n    a" @?= Right "do { f a }"
  , testCase "do"                $ tokenize "do\nf"       @?= Right "do { f }"
  , testCase "do"                $ tokenize "do\nf\na"    @?= Right "do { f ; a }"

  , testCase "do"                $ tokenize "f f do f"         @?= Right "f f do { f }"
  , testCase "do"                $ tokenize "f f do\n       f" @?= Right "f f do { f }"
  , testCase "do"                $ tokenize "f f do\n  f"      @?= Right "f f do { f }"
  ]

tokenize = fmap unwords . strides atom intro "{" "}" ";"

atom :: P (Tree String)
atom = choice (map (fmap Sym . string) ["f", "a", "b"]) <* spaces

intro :: P String
intro = string "do" <* spaces
