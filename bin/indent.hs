module Main where

import Data.List (head)
import Data.String
import qualified Data.Text as T
import GHC.Exts (IsString(..))
import Protolude hiding (empty, head, list, many, sourceColumn, sourceLine, sym, try, unwords, words, Associativity, First, Infix, LeftAssociative, Prefix, Last, RightAssociative, (<|>))
import System.Environment (getArgs)
import Text.ParserCombinators.Parsec
import Text.Syntactical
import Text.Syntactical.Indent (Tree(..), strides')


--------------------------------------------------------------------------------
-- Simple command-line program
-- -i   just show the result of the tokenizing (indentation)
-- -fi  idem on a file
-- -f   apply the shunting yard to the file
--      apply the shunting yard to the argument
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-i", s] -> case tokenize s of
      Right a -> putStrLn . unwords $ map toString a
      Left err -> putStrLn $ "indentation error: " ++ show err
    ["-fi", fn] -> do
      s <- readFile fn
      case tokenize $ T.unpack s of
        Right a -> putStrLn . unwords $ map toString a
        Left err -> putStrLn $ "indentation error: " ++ show err
    ["-f", fn] -> do
      s <- readFile fn
      case tokenize $ T.unpack s of
        Right a -> case shunt table0 . map Atom $ a of
          Right e -> putStrLn $ showSExpr e
          Left f -> putStrLn $ showFailure f
        Left err -> putStrLn $ "indentation error: " ++ show err
    [s] -> case tokenize s of
      Right a -> case shunt table0 . map Atom $ a of
        Right e -> putStrLn $ showSExpr e
        Left f -> putStrLn $ showFailure f
      Left err -> putStrLn $ "indentation error: " ++ show err
    _ -> putStrLn @Text "Usage: (TODO)"


----------------------------------------------------------------------
-- The token type for Syntactical
----------------------------------------------------------------------

data MyToken = MyToken Source String

data Source = Source Int Int -- source file line and column
            | Internal       -- generated

instance IsString MyToken where
  fromString = MyToken Internal

instance Token MyToken where
  toString (MyToken _ t) = t
  operator = myOperator
  consider (MyToken _ a) (MyToken _ b) = a == b

-- Rewrite the sub-expressions as we apply the operator.
myOperator o as = case pts of
  "()" -> case as of
    [List [Atom (MyToken Internal ","),a,b]] ->
      tuple [] (head as)
    [as'] -> as'
  "[]" -> case as of
    [as'] -> list "," [Atom (MyToken Internal "list")] as'
  "{}" -> case as of
    [as'] -> list ";" [Atom (MyToken Internal "declarations")] as'
  "``" -> case as of
    [a,b,c] -> List [b,a,c]
  _ -> List $ (Atom $ MyToken Internal pts):as
  where pts = concatMap toString $ symbols o

tuple xs (List [Atom (MyToken Internal ","),a,b]) = tuple (a:xs) b
tuple xs b = List (a : reverse (b:xs))
  where a = Atom (MyToken Internal $ ',' : show (length xs + 1))

list c xs (List [Atom (MyToken Internal c'),a,b]) | c == c' = list c (a:xs) b
list _ xs b = List (reverse (b:xs))

----------------------------------------------------------------------
-- The operator table for Syntactical
----------------------------------------------------------------------

table0 :: Table MyToken
table0 = buildTable
 [ [ closed "(" Distfix ")"
   , closed_ "⟨" SExpression "⟩"
   , closed "[" Distfix "]"
   , closed "[" Distfix "|" `distfix` "]"
   , closed "{" Distfix "}"
   ]
 , [ infx RightAssociative "?'" `distfix` ":'"
   , postfx "!"
   , postfx "_/" `distfix` "/."
   ]
 , [ postfx "%"
   , prefx "#"
   ]
 , [ postfx "°"
   , infx LeftAssociative "*"
   , infx LeftAssociative "/"
   ]
 , [ infx LeftAssociative "+"
   , infx LeftAssociative "-"
   ]
 , [ infx LeftAssociative "<<"
   , infx LeftAssociative ">>"
   , infx RightAssociative "?" `distfix` ":"
   ]
 , [ infx LeftAssociative "`" `distfix` "`"
   ]
 , [ prefx "if" `distfix` "then" `distfix` "else"
   ]
 , [ infx RightAssociative "->"
   ]
 , [ infx RightAssociative ","
   ]
 , [ prefx "let" `distfix` "in"
   , infx RightAssociative "where"
   , prefx "case" `distfix` "of"
   ]
 , [ prefx "\\" `distfix` "->"
   , prefx "λ" `sexpr` "."
   ]
 , [ infx RightAssociative "::"
   , infx RightAssociative "="
   ]
 , [ infx RightAssociative ";"
   ]
 ]

----------------------------------------------------------------------
-- Tokenizing, using a simple indentation scheme (provided by the
-- Indent module).
----------------------------------------------------------------------

type P a = GenParser Char () a

source :: P Source
source = do
  p <- getPosition
  let l = sourceLine p
      c = sourceColumn p
  return $ Source l c

keywords :: [String]
keywords = words "let where of"

tokenize = strides' atom intro "{" "}" ";"

-- Parse an atom.
atom :: P (Tree MyToken)
atom = empty <|> str <|> sym

-- Parse a keyword that introduces an indentation level.
intro :: P MyToken
intro = do
  src <- source
  str <- choice (map string keywords)
  return $ MyToken src str

-- Parse a symbol. A symbol is any consecutive list of non-blank
-- characters except for `,()⟨⟩[], which are each a single symbol.
sym :: P (Tree MyToken)
sym = try $ do
  src <- source
  x <- noneOf "\t\n "
  if x `elem` ("`,()⟨⟩[]" :: String)
    then spaces >> return (Sym $ MyToken src [x])
    else do
      xs <- manyTill anyChar (lookAhead $ (oneOf "`,()⟨⟩[]\t\n " >> return ()) <|> eof)
      if (x:xs) `elem` keywords
        then pzero
        else spaces >> return (Sym . MyToken src $ x:xs)

-- Parse the empty-list symbol.
empty :: P (Tree MyToken)
empty = try $ do
  src <- source
  _ <- char '['
  spaces
  _ <- char ']'
  spaces
  return . Sym $ MyToken src "[]"

-- Parse a string literal.
str :: P (Tree MyToken)
str = try $ do
  src <- source
  _ <- char '"'
  x <- many (noneOf "\t\n\"")
  _ <- char '"'
  spaces
  return . Sym $ MyToken src ('"' : x ++ "\"")
