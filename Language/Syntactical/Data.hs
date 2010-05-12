module Language.Syntactical.Data where

import Data.List

data Tree = Node [Tree]
-- The to-be-shunted tokens. Only the information for the
-- shunting yard algorithm is represented. Actual tokens should
-- be converted to this representation.
           | Num Int
           | Sym String
           | Op [String] -- on the stack, TODO turn into Sym on the output
  deriving Eq

data Op = Infix [String] [String] Associativity Precedence -- infix
        | Prefix [String] [String] Precedence -- prefix
        | Postfix [String] [String] Precedence -- postfix
        | Closed [String] [String] Kind
        -- TODO SExpression so the user can choose the brackets for s-expr
  deriving Show

data Kind = Discard | Keep | SExpression | Distfix | DistfixAndDiscard
  deriving Show

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

associativity (Infix _ _ a _) = a

prec (Infix _ _ _ p) = p

nonAssoc = (NonAssociative ==) . associativity
lAssoc = (LeftAssociative ==) . associativity
rAssoc = (RightAssociative ==) . associativity

isInfix (Infix _ _ _ _) = True
isInfix _ = False

isInfix' (Infix xs _ _ _) ys = xs == ys
isInfix' _ _ = False

lower o1@(Infix [_] _ _ _) o2@(Infix _ [] _ _)
    | nonAssoc o1 || (lAssoc o1 && prec o1 <= prec o2) = True
    | rAssoc o1 && prec o1 < prec o2 = True
lower _ _ = False

findOp op (Table t) = findOp' op t

findOp' op [] = []
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

findOps ops (Table t) = findOps' ops t

findOps' ops [] = []
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

break' p ls = case break p ls of
  (_, []) -> error "break': no element in l satisfying p"
  (l, r) -> (l ++ [head r], tail r)

