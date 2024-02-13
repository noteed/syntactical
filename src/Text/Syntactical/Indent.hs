-- Simple indentation parser for Parsec. It constructs blocks of list of
-- continued lines (called strides). It is parametrized by the strings
-- that introduce indented blocks and by the parser for the leaves of the
-- tree.
module Text.Syntactical.Indent where

import Data.String
import Protolude hiding (try, sourceColumn, sourceLine, (<|>))
import Text.ParserCombinators.Parsec

-- A tree data structure to represent indentation.
data Tree a =
    Sym a
  | Block a [Stride a] -- Block s ss represents an indented block
                       -- introduced by the string s.
  deriving Show

newtype Stride a = Stride [Tree a]
  deriving Show

-- Flatten a list of strides (that is a block) into a list of
-- token, introducing indent, dedent, and sequence tokens.
flatten :: a -> a -> a -> [Stride a] -> [a] -> [a]
flatten i d sq = symStrides
  where
    symStrides [s] = symStride s
    symStrides (s:ss) = symStride s . (sq :) . symStrides ss

    symStride (Stride ts) = symTrees ts

    symTrees [] = identity
    symTrees (t:ts) = symTree t . symTrees ts

    symTree (Sym x) = (x :)
    symTree (Block name ss) =
      (\a -> name : i : a) .
      symStrides ss .
      (d :)

type Pos = (Int,Int)

type P a = GenParser Char () a

-- Return the current source line and source column.
getPos :: P Pos
getPos = do
  p <- getPosition
  let l = sourceLine p
      c = sourceColumn p
  return (l,c)

-- Continue parsing (using p) after position (l1,c1).
continue :: Pos -> P a -> P a
continue (l1,c1) p = do
  (l2,c2) <- getPos
  unless (c1 < c2 || l1 == l2) pzero
  p

-- aligned p parses many1 p with all p aligned on the same column.
aligned :: P a -> P [a]
aligned p = do
  -- get the defining column
  (_,dc) <- getPos
  -- many1 p but only with p starting exactly at dc
  many1 (getPos >>= \(_,c) -> unless (c == dc) pzero >> p)

-- Parse one of the given strings then a block.
indent :: P (Tree a) -> P a -> P (Tree a)
indent atom intro = try $ do
  (l1, c1) <- getPos
  s <- intro
  spaces
  (l2, c2) <- getPos
  b <- block atom intro
  spaces
  (l3, c3) <- getPos
  -- We check that if a block starts further on the right that the introducing
  -- keyword (c1 < c2), then when it's done being parsed, the following tokens
  -- can't start in between the introducing keyword and the block.
  -- This is to prevent someting like
  --
  --   ... = do f
  --           a
  when (c1 < c2 && c1 < c3 && c3 < c2) pzero
  return $ Block s b

-- Parse a single (possibly nested) symbol, where the nesting can be
-- introduced by one of the given tokens.
tree :: P (Tree a) -> P a -> P (Tree a)
tree atom intro = atom <|> indent atom intro

-- Parse a continued list of (possibly nested) symbols, where the
-- nesting can be introduced by one of the given tokens.
stride :: P (Tree a) -> P a -> P (Stride a)
stride atom intro =
  getPos >>= many1 . flip continue (tree atom intro) >>= return . Stride

-- Parse a non-empty sequence of verticaly-aligned strides. Nested
-- blocks can be introduce by one of the given tokens.
block :: P (Tree a) -> P a -> P [Stride a]
block atom intro = aligned (stride atom intro)

-- The top-level parser to parse a non-empty sequence of strides.
-- Nested blocks can be introduce by one of the given tokens.
strides_ :: P (Tree a) -> P a -> String -> Either ParseError [Stride a]
strides_ atom intro = parse (spaces >> block atom intro <* eof) "strides"

-- The top-level parser to parse a non-empty sequence of
-- strides and return them already flattened, using the indent,
-- dedent, and sequence tokens i, d and sq.
-- Nested blocks can be introduce by one of the given tokens.
strides :: P (Tree a) -> P a -> a -> a -> a -> String -> Either ParseError [a]
strides atom intro i d sq =
  fmap (flip (flatten i d sq) []) . strides_ atom intro

-- Same as above but wrap the result in the indent/dedent tokens.
strides' :: P (Tree a) -> P a -> a -> a -> a -> String -> Either ParseError [a]
strides' atom intro i d sq =
  fmap ((i:) . flip (flatten i d sq) [d]) . strides_ atom intro
