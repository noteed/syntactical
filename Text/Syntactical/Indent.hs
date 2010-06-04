module Text.Syntactical.Indent where

import Text.ParserCombinators.Parsec
import Control.Monad (unless)

-- indentation handling

type Pos = (Int,Int)

getPos :: GenParser Char st Pos
getPos = do
  p <- getPosition
  let l = sourceLine p
      c = sourceColumn p
  return (l,c)

onside :: Pos -> Pos -> Bool
onside (l1,c1) (l2,c2) = c1 < c2 || l1 == l2

offside :: Pos -> GenParser Char st a -> GenParser Char st a
offside pos p = do
  pos' <- getPos
  unless (onside pos pos') pzero
  p

off :: Pos -> GenParser Char st a -> GenParser Char st a
off (_,dc) p = do
  (_,c) <- getPos
  unless (c == dc) pzero
  p

offsideMany1 :: GenParser Char st a -> GenParser Char st [a]
offsideMany1 p = do
  pos <- getPos
  many1 (off pos p)

data Tree =
    Sym String
  | Let [Stride] Stride
  | Where [Stride] -- not e1 where e2 ; en
  | Case Stride [Stride]
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
    symTree (Case s ss) =
      ("case" :) .
      symStride s .
      (\a -> "of" : "{" : a) .
      symStrides ss .
      ("}" :)

keywords :: [String]
keywords = words "let in where case of"

sym :: GenParser Char st Tree
sym = try $ do
  x <- noneOf "\t\n "
  if x `elem` "()⟨⟩"
    then spaces >> return (Sym [x])
    else do
      xs <- manyTill anyChar (lookAhead $ (oneOf ",()⟨⟩\t\n " >> return ()) <|> eof)
      if (x:xs) `elem` keywords then pzero else spaces >> return (Sym $ x:xs)

str :: GenParser Char st Tree
str = try $ do
  _ <- char '"'
  x <- many (noneOf "\t\n\"")
  _ <- char '"'
  spaces
  return $ Sym ('"' : x ++ "\"")

letin :: GenParser Char st Tree
letin = try $ do
  string "let" >> spaces
  b <- parseBlock
  string "in" >> spaces
  s <- parseStride
  return $ Let b s

caseof :: GenParser Char st Tree
caseof = try $ do
  string "case" >> spaces
  s <- parseStride
  string "of" >> spaces
  b <- parseBlock
  return $ Case s b

wher :: GenParser Char st Tree
wher = try $ do
  string "where" >> spaces
  b <- parseBlock
  return $ Where b

parseTree :: Pos -> GenParser Char st Tree
parseTree pos = offside pos (str <|> sym <|> letin <|> wher <|> caseof)

parseStride :: GenParser Char st Stride
parseStride = getPos >>= many1 . parseTree >>= return . Stride

parseBlock :: GenParser Char st [Stride]
parseBlock = offsideMany1 parseStride

go :: [Char] -> Either ParseError Stride
go = parse (spaces >> parseStride) "parseStride"

go' :: [Char] -> Either ParseError [Stride]
go' = parse (spaces >> parseBlock) "parseBlock"

