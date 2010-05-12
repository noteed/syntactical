module Language.Syntactical.Indent where

import Text.ParserCombinators.Parsec
import Control.Monad (unless)
import Data.List (intersperse)

-- indentation handling

type Pos = (Int,Int)

getPos = do
  p <- getPosition
  let l = sourceLine p
      c = sourceColumn p
  return (l,c)

onside (l1,c1) (l2,c2) = c1 < c2 || l1 == l2

offside pos p = do
  pos' <- getPos
  unless (onside pos pos') pzero
  p

off (_,dc) p = do
  (_,c) <- getPos
  unless (c == dc) pzero
  p

offsideMany1 p = do
  pos <- getPos
  many1 (off pos p)

data Tree =
    Sym String
  | Let [Stride] Stride
  | L
  | R
  deriving Show

data Stride = Stride [Tree]
  deriving Show

symStrides [s] = symStride s
symStrides (s:ss) = symStride s . (Sym "SEP" :) . symStrides ss

symStride (Stride ts) = symTrees ts

symTrees [] = id
symTrees (t:ts) = symTree t . symTrees ts

symTree (Sym x) = (Sym x :)
symTree (Let ss s) =
  (\a -> Sym "let" : Sym "BEGIN" : a) .
  symStrides ss .
  (\a -> Sym "END" : Sym "in" : a) .
  symStride s
symTree L = (L :)
symTree R = (R :)

keywords = words "let in"

atom = try $ do
  x <- many1 alphaNum
  if x `elem` keywords then pzero else spaces >> return (Sym x)

letin = try $ do
  (ll,lc) <- getPos
  string "let" >> spaces
  b <- parseBlock
  (il,ic) <- getPos
  string "in" >> spaces
  unless (il == ll || ic == lc) $ fail "let-in not aligned"
  s <- parseStride
  return $ Let b s

lparen = char '(' >> spaces >> return L

rparen = char ')' >> spaces >> return R

parseTree pos = offside pos (atom <|> letin <|> lparen <|> rparen)

parseStride = getPos >>= many1 . parseTree >>= return . Stride

parseBlock = offsideMany1 parseStride

go = parse (spaces >> parseStride) "parseStride"

go' = parse (spaces >> parseBlock) "parseBlock"

