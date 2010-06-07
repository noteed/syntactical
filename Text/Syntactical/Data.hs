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

infx :: String -> [String] -> Associativity -> Op
infx f rest a = Infix [] (f:rest) a 0

prefx :: String -> [String] -> Op
prefx f rest = Prefix [] (f:rest) 0

postfx :: String -> [String] -> Op
postfx f rest = Postfix [] (f:rest) 0

closed :: String -> [String] -> String -> Kind -> Op
closed f rest l k = Closed [] (f:rest++[l]) k

setPrecedence :: Precedence -> Op -> Op
setPrecedence p (Infix l r a _) = Infix l r a p
setPrecedence p (Prefix l r _) = Prefix l r p
setPrecedence p (Postfix l r _) = Postfix l r p
setPrecedence _ c = c

-- buildTable constructs an operator table that can be
-- used with the shunt function. Operators are given
-- in decreasing precedence order.
-- TODO see if Parsec's buildExpressionParser uses a
-- incresing or decreasing list.
buildTable :: [[Op]] -> Table
buildTable ls = Table . concat $ zipWith f ls [n, n - 1 .. 0]
  where n = length ls
        f l p = map (setPrecedence p) l

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

isPrefix :: Op -> Bool
isPrefix (Prefix _ _ _) = True
isPrefix _ = False

isPostfix :: Op -> Bool
isPostfix (Postfix _ _ _) = True
isPostfix _ = False

isClosed :: Op -> Bool
isClosed (Closed _ _ _) = True
isClosed _ = False

isInfix' :: Op -> [String] -> Bool
isInfix' (Infix xs _ _ _) ys = xs == ys
isInfix' _ _ = False

isSExpression :: Op -> Bool
isSExpression (Closed _ _ SExpression) = True
isSExpression _ = False

continue :: Op -> Op -> Bool
continue (Infix l1 _ _ _) (Infix l2 (r2:_) _ _) = l2++[r2] == l1
continue (Prefix l1 _ _) (Prefix l2 (r2:_) _) = l2++[r2] == l1
continue (Postfix l1 _ _) (Postfix l2 (r2:_) _) = l2++[r2] == l1
continue (Closed l1 _ _) (Closed l2 (r2:_) _) = l2++[r2] == l1
continue _ _ = False

full :: Op -> Bool
full (Infix _ [] _ _) = True
full (Prefix _ [] _) = True
full (Postfix _ [] _) = True
full (Closed _ [] _) = True
full _ = False

lower :: Op -> Op -> Bool
lower (Infix [_] _ a1 p1) (Infix _ [] a2 p2)
    | a1 == NonAssociative && a2 == NonAssociative = error "cannot mix"
    | a1 == LeftAssociative && p1 <= p2 = True
    | a1 == RightAssociative && p1 < p2 = True
    | a1 == NonAssociative && p1 <= p2 = True
    | otherwise = False
lower (Infix [_] _ a1 p1) (Prefix _ [] p2)
    | a1 == NonAssociative = error "cannot mix"
    | a1 == LeftAssociative && p1 <= p2 = True
    | a1 == RightAssociative && p1 < p2 = True
    | otherwise = False
lower (Postfix [_] _ p1) (Prefix _ [] p2)
    | p1 == p2 = error "cannot mix"
    | p1 < p2 = True
    | otherwise = False
lower (Postfix [_] _ p1) (Infix _ [] a2 p2)
    | a2 == NonAssociative = error "cannot mix"
    | a2 == LeftAssociative && p1 <= p2 = True
    | a2 == RightAssociative && p1 < p2 = True
    | otherwise = False
lower (Prefix _ [] _) _ = True
lower (Postfix _ [] _) _ = True
lower (Closed _ [] _) _ = True
lower o1 _ | part o1 == Middle = True
lower _ _ = False

findOp :: String -> Table -> [Op]
findOp op (Table t) = findOp' op t

findOp' :: String -> [Op] -> [Op]
findOp' _ [] = []
findOp' op (Infix [] pts a p:xs)
  | op `elem` pts =
     let (l,r) = break' (== op) pts
     in Infix l r a p : findOp' op xs
  | otherwise = findOp' op xs
findOp' op (Prefix [] pts p:xs)
  | op `elem` pts =
     let (l,r) = break' (== op) pts
     in Prefix l r p : findOp' op xs
  | otherwise = findOp' op xs
findOp' op (Postfix [] pts p:xs)
  | op `elem` pts =
     let (l,r) = break' (== op) pts
     in Postfix l r p : findOp' op xs
  | otherwise = findOp' op xs
findOp' op (Closed [] pts k:xs)
  | op `elem` pts =
     let (l,r) = break' (== op) pts
     in Closed l r k : findOp' op xs
  | otherwise = findOp' op xs
findOp' _ _ = error "findOps called on malformed operator table"

findOps :: [String] -> Table -> [Op]
findOps ops (Table t) = findOps' ops t

findOps' :: [String] -> [Op] -> [Op]
findOps' _ [] = []
findOps' ops (Infix [] pts a p:xs)
  | ops `isPrefixOf` pts = Infix ops (drop (length ops) pts) a p : findOps' ops xs
  | otherwise = findOps' ops xs
findOps' ops (Prefix [] pts p:xs)
  | ops `isPrefixOf` pts = Prefix ops (drop (length ops) pts) p : findOps' ops xs
  | otherwise = findOps' ops xs
findOps' ops (Postfix [] pts p:xs)
  | ops `isPrefixOf` pts = Postfix ops (drop (length ops) pts) p : findOps' ops xs
  | otherwise = findOps' ops xs
findOps' ops (Closed [] pts k:xs)
  | ops `isPrefixOf` pts = Closed ops (drop (length ops) pts) k : findOps' ops xs
  | otherwise = findOps' ops xs
findOps' _ _ = error "findOps called on malformed operator table"

break' :: (a -> Bool) -> [a] -> ([a], [a])
break' p ls = case break p ls of
  (_, []) -> error "break': no element in l satisfying p"
  (l, r:rs) -> (l ++ [r], rs)

applicator :: Table -> Tree -> Bool
applicator table (Sym x) = findOp x table == []
applicator _ (Node _) = True
applicator _ _ = False

findOperators :: Table -> String -> [String] -> (Op, Op)
findOperators table x y =
 (head $ findOp x table, head $ findOps y table)

-- Parts
-- Examples:
-- if_then_else_ : Prefix
-- \    \    \
--  \    \    - Last True   - last part with a hole on its right,
--   \    \                   the hole on its left is implied.
--    \    ---- Middle      - middle part, the two holes are implied.
--     -------- First False - first part with no hole on its left,
--                            the hole on its right is implied.
--
-- _+_ : Infix
--  \
--   - Lone BothOpen  - first and last part, with two holes.
--
-- _! : Postfix
--   \
--    - Lone LeftOpen - first and last part, with a left hole.

data Part = First Bool
          | Last Bool
          | Lone Opening
          | Middle
  deriving (Show, Eq)

data Opening = LeftOpen
             | RightOpen
             | BothOpen
  deriving (Show, Eq)

leftHole :: Part -> Bool
leftHole (First True) = True
leftHole (First _) = False
leftHole (Last _) = True
leftHole (Lone RightOpen) = False
leftHole (Lone _) = True
leftHole Middle = True

rightHole :: Part -> Bool
rightHole (First _) = True
rightHole (Last True) = True
rightHole (Last _) = False
rightHole (Lone LeftOpen) = False
rightHole (Lone _) = True
rightHole Middle = True

part :: Op -> Part

part (Infix [] _ _ _) = error "part called on malformed infix operator"
part (Infix [_] [] _ _) = Lone BothOpen
part (Infix [_] _ _ _) = First True
part (Infix _ [] _ _) = Last True
part (Infix _ _ _ _) = Middle

part (Prefix [] _ _) = error "part called on malformed prefix operator"
part (Prefix [_] [] _) = Lone RightOpen
part (Prefix [_] _ _) = First False
part (Prefix _ [] _) = Last True
part (Prefix _ _ _) = Middle

part (Postfix [] _ _) = error "part called on malformed postfix operator"
part (Postfix [_] [] _) = Lone LeftOpen
part (Postfix [_] _ _) = First True
part (Postfix _ [] _) = Last False
part (Postfix _ _ _) = Middle

part (Closed [] _ _) = error "part called on malformed closed operator"
part (Closed [_] [] _) = error "part called on malformed closed operator"
part (Closed [_] _ _) = First False
part (Closed _ [] _) = Last False
part (Closed _ _ _) = Middle

parts :: Op -> ([String], [String])
parts (Infix l r _ _) = (l,r)
parts (Prefix l r _) = (l,r)
parts (Postfix l r _) = (l,r)
parts (Closed l r _) = (l,r)

