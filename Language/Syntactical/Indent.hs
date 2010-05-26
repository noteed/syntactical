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
  | Where [Stride] -- not e1 where e2 ; en
  deriving Show

data Stride = Stride [Tree]
  deriving Show

flatten :: [Stride] -> [String] -> [String]
flatten = symStrides
  where
    symStrides [s] = symStride s
    symStrides (s:ss) = symStride s . (";" :) . symStrides ss

    symStride (Stride ts) = symTrees ts

    symTrees [] = id
    symTrees (t:ts) = symTree t . symTrees ts

    symTree (Sym x) = (x :)
    symTree (Let ss s) =
      (\a -> "let" : "{" : a) .
      symStrides ss .
      (\a -> "}" : "in" : a) .
      symStride s
    symTree (Where ss) =
      (\a -> "where" : "{" : a) .
      symStrides ss .
      ("}" :)

keywords = words "let in where"

atom = try $ do
  x <- many1 alphaNum
  if x `elem` keywords then pzero else spaces >> return (Sym x)

letin = try $ do
  (ll,lc) <- getPos
  string "let" >> spaces
  b <- parseBlock
  (il,ic) <- getPos
  string "in" >> spaces
  s <- parseStride
  return $ Let b s

wher = try $ do
  string "where" >> spaces
  b <- parseBlock
  return $ Where b

lparen = char '(' >> spaces >> return (Sym "(")

rparen = char ')' >> spaces >> return (Sym ")")

parseTree pos = offside pos (atom <|> letin <|> wher <|> lparen <|> rparen)

parseStride = getPos >>= many1 . parseTree >>= return . Stride

parseBlock = offsideMany1 parseStride

go = parse (spaces >> parseStride) "parseStride"

go' = parse (spaces >> parseBlock) "parseBlock"

