module Text.Syntactical.Data where

import Data.List

data Tree = Node [Tree]
-- The to-be-shunted tokens. Only the information for the
-- shunting yard algorithm is represented. Actual tokens should
-- be converted to this representation.
-- TODO if the above is true, then algorithm should return the
-- original data instead of a Tree.
           | Num Int
           | Sym String
           | Op [String] -- on the stack, TODO turn into Sym on the output
  deriving Eq

data Op = Infix [String] [String] Associativity Precedence
        | Prefix [String] [String] Precedence
        | Postfix [String] [String] Precedence
        | Closed [String] [String] Kind
  deriving (Eq, Show)

-- The Kind is used to give various behaviours when dealing
-- with Closed operators.
-- Discard means the Closed operator will be removed from the
-- resulting Tree.
-- Keep is the opposite of Discard.
-- SExpression means the 'content' of the Closed operator
-- should be parsed as an s-expression.
-- Distfix means the 'content' of the Closed operator
-- should be parsed as a distfix expression.
-- DistfixAndDiscard combines the meaning of both Discard and
-- Distfix.
data Kind = Discard | Keep | SExpression | Distfix | DistfixAndDiscard
  deriving (Eq, Show)

data Associativity = NonAssociative | LeftAssociative | RightAssociative
  deriving (Show, Eq)

type Precedence = Int

data Table = Table [Op]

instance Show Tree where
  show = display

display :: Tree -> String
display = tail . display'
  where
  display' (Num i) = ' ' : show i
  display' (Sym s) = ' ' : s
  display' (Op l) = ' ' : concat l
  display' (Node es) = ' ' : '⟨' : tail (concatMap display' es) ++ "⟩"

isSym :: Tree -> Bool
isSym (Sym _) = True
isSym _ = False

isOp :: Tree -> Bool
isOp (Op _) = True
isOp _ = False

isInfix :: Op -> Bool
isInfix (Infix _ _ _ _) = True
isInfix _ = False

isInfix' :: Op -> [String] -> Bool
isInfix' (Infix xs _ _ _) ys = xs == ys
isInfix' _ _ = False

lower :: Op -> Op -> Bool
lower (Infix [_] _ a1 p1) (Infix _ [] a2 p2)
    | a1 == NonAssociative && a2 == NonAssociative = error "cannot mix"
    | a1 == LeftAssociative && p1 <= p2 = True
    | a1 == RightAssociative && p1 < p2 = True
    | a1 == NonAssociative && p1 <= p2 = True
lower (Infix [_] _ a1 p1) (Prefix _ [] p2)
    | a1 == NonAssociative = error "cannot mix"
    | a1 == LeftAssociative && p1 <= p2 = True
    | a1 == RightAssociative && p1 < p2 = True
    | a1 == NonAssociative && p1 <= p2 = True
lower _ _ = False

findOp :: String -> Table -> [Op]
findOp op (Table t) = findOp' op t

findOp' :: String -> [Op] -> [Op]
findOp' _ [] = []
findOp' op (Infix [] parts a p:xs)
  | op `elem` parts =
     let (l,r) = break' (== op) parts
     in Infix l r a p : findOp' op xs
  | otherwise = findOp' op xs
findOp' op (Prefix [] parts p:xs)
  | op `elem` parts =
     let (l,r) = break' (== op) parts
     in Prefix l r p : findOp' op xs
  | otherwise = findOp' op xs
findOp' op (Postfix [] parts p:xs)
  | op `elem` parts =
     let (l,r) = break' (== op) parts
     in Postfix l r p : findOp' op xs
  | otherwise = findOp' op xs
findOp' op (Closed [] parts k:xs)
  | op `elem` parts =
     let (l,r) = break' (== op) parts
     in Closed l r k : findOp' op xs
  | otherwise = findOp' op xs
findOp' _ _ = error "findOps called on malformed operator table"

findOps :: [String] -> Table -> [Op]
findOps ops (Table t) = findOps' ops t

findOps' :: [String] -> [Op] -> [Op]
findOps' _ [] = []
findOps' ops (Infix [] parts a p:xs)
  | ops `isPrefixOf` parts = Infix ops (drop (length ops) parts) a p : findOps' ops xs
  | otherwise = findOps' ops xs
findOps' ops (Prefix [] parts p:xs)
  | ops `isPrefixOf` parts = Prefix ops (drop (length ops) parts) p : findOps' ops xs
  | otherwise = findOps' ops xs
findOps' ops (Postfix [] parts p:xs)
  | ops `isPrefixOf` parts = Postfix ops (drop (length ops) parts) p : findOps' ops xs
  | otherwise = findOps' ops xs
findOps' ops (Closed [] parts k:xs)
  | ops `isPrefixOf` parts = Closed ops (drop (length ops) parts) k : findOps' ops xs
  | otherwise = findOps' ops xs
findOps' _ _ = error "findOps called on malformed operator table"

break' :: (a -> Bool) -> [a] -> ([a], [a])
break' p ls = case break p ls of
  (_, []) -> error "break': no element in l satisfying p"
  (l, r) -> (l ++ [head r], tail r)

