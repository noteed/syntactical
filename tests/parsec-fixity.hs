-- Some definitions to experiment with Parsec's buildExpressionParser
-- associativity and precedence parsing rules.
-- It works similarly to GHC (see ghc-fixity.hs) but doesn't give
-- a parse error, simply fails when it can't mix operators.

import Text.Parsec
import Text.Parsec.Expr

data Expr = X | Y | Z
          | A Expr Expr
          | B Expr Expr
  deriving (Read, Show)

go :: String -> Either ParseError Expr
go = parse expr "parsec-fixity"

expr = buildExpressionParser table term
     <?> "expression"

term = spaces >> read `fmap` choice (map string ["X","Y","Z"])
     <?> "term"

table =
 [ [ binary "al7" A AssocLeft
   , binary "ar7" A AssocRight
   ]
 , [ binary "al6" A AssocLeft
   , binary "ar6" A AssocRight
   , binary "a6" A AssocNone
   , binary "bl6" B AssocLeft
   , binary "br6" B AssocRight
   , binary "b6" B AssocNone
   ]
 , [ binary "al5" A AssocLeft
   , binary "a5" A AssocNone
   , binary "b5" B AssocNone
   ]
  ]

binary name fun assoc =
  Infix (try $ do{ spaces >> string name; return fun }) assoc

