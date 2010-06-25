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

data Op =
    Infix
  { opL :: [String]
  , opR :: [String]
  , opA :: Associativity
  , opP :: Precedence
  }
  | Prefix
  { opL :: [String]
  , opR :: [String]
  , opP :: Precedence
  }
  | Postfix
  { opL :: [String]
  , opR :: [String]
  , opP :: Precedence
  }
  | Closed
  { opL :: [String]
  , opR :: [String]
  , opK :: Kind
  }
  deriving (Eq, Show)

-- Alternative data type to represent operators.
-- The Kind specifies how the hole sould be parsed:
-- either as an s-expression (missing the brackets) or as
-- a distfix expression. Maybe it could be extended to
-- support user-defined parsers, ot at least a specific
-- operator table.
-- The boolean is to specify if the operator should show up
-- in the result or be discarded.
-- e.g. ⟨1 2⟩ instead of ⟨+ 1 2⟩ when the + is retained.
-- e.g. 1 instead of ⟨! 1⟩ when the ! is retained.
-- e.g. ⟨1⟩ instead of ⟨! 1⟩ if the hole is also parsed as an s-expr.
data OpX = Op1 Bool String [(Kind,String)] Opening Associativity Precedence
         | Op2 Bool String Kind String [(Kind,String)]

-- Internal representation. This representation can have non-sensical
-- values. The code will not create them and the user will not be able
-- to create them either.
-- (left hole?, either, associativity, precedence, possible preceding
-- part, parts, possible following part)
-- where the either value is left kind when the operator isn't complete (and the
-- kind gives information about the rigth hole) or is right bool when the operator
-- is complete (and the (bool,bool) specifies if it is kept and if there is a hole on the right).
-- Left hole is always True when there is more than one part.
data InternalOp =
  InternalOp Bool (Either Kind (Bool,Bool)) Associativity Precedence [String] String [String]

buildInternalOp :: Op -> InternalOp
buildInternalOp (Infix l r a p) = InternalOp
  True (if null r then Right (True,True) else Left Distfix) a p [last $ init l] (last l) [head r]
buildInternalOp (Prefix l r p) = InternalOp
  False (if null r then Right (True,True) else Left Distfix) LeftAssociative p [last $ init l] (last l) [head r]
buildInternalOp (Postfix l r p) = InternalOp
  True (if null r then Right (True,False) else Left Distfix) RightAssociative p [last $ init l] (last l) [head r]
buildInternalOp (Closed l r k) = InternalOp
  False (if null r then Right (a,False) else Left b) NonAssociative 0 [last $ init l] (last l) [head r]
  where (a,b) = case k of
          Discard -> (False,Distfix)
          Keep -> (True,Distfix)
          SExpression -> (False,SExpression)
          Distfix -> (True,Distfix)
          DistfixAndDiscard -> (False,Distfix)

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
lower o1 _ | isMiddle (part o1)
           || isLast (part o1) && not (isLone $ part o1)= True
lower _ _ = False

findOp :: String -> Table -> [Op]
findOp op (Table t) = findOp' op t

findOp' :: String -> [Op] -> [Op]
findOp' _ [] = []
findOp' op (o:os) =
  case parts o of
   ([], pts) ->
     if op `elem` pts
     then let (l,r) = break' (== op) pts
          in o { opL = l , opR = r } : findOp' op os
     else findOp' op os
   _ -> error "findOp called on malformed operator table"

findOps :: [String] -> Table -> [Op]
findOps ops (Table t) = findOps' ops t

findOps' :: [String] -> [Op] -> [Op]
findOps' _ [] = []
findOps' ops (o:os) =
  case parts o of
   ([], pts) ->
     if ops `isPrefixOf` pts
     then o { opL = ops , opR = drop (length ops) pts } : findOps' ops os
     else findOps' ops os
   _ -> error "findOps called on malformed operator table"

break' :: (a -> Bool) -> [a] -> ([a], [a])
break' p ls = case break p ls of
  (_, []) -> error "break': no element in l satisfying p"
  (l, r:rs) -> (l ++ [r], rs)

applicator :: Table -> Tree -> Bool
applicator table (Sym x) = findOp x table == []
applicator _ (Node _) = True
applicator _ _ = False

-- findOperators makes it possible to use a symbol
-- as part of multiple operators. For now the rule
-- is simple: two or more operators can have the
-- same prefix if they differ by at least one symbol
-- after the prefix. Also the first part of the operators
-- should have identical leftHole value.
-- Examples:
-- , and [_,_] are ambiguous
-- < and <_> are ambiguous
-- [_] and [_,_] are permitted
-- _::_; and _=_; are permitted
-- For now, the fixity of the possible operators should
-- be the same although it is possible to, e.g., allow
-- /_/ and /_\_
findOperators :: Table -> String -> [String] -> (Op, Op)
findOperators table x y =
  if nonAmbiguous xs && nonAmbiguous ys
  then if null xy then (head xs, head ys) else head xy
  else if nonAmbiguous xs then error $ "ambiguous operators " ++ show ys
                     else error $ "ambiguous operators " ++ show xs
  where xs = findOp x table
        ys = findOps y table
        xy = [(a,b) | a <- xs, b <- ys, x `continue` (part b)]

-- TODO check precedence/associativity
nonAmbiguous :: [Op] -> Bool
nonAmbiguous [] = True
nonAmbiguous (o:os) = case o of
  Infix _ _ _ _ -> all isInfix os
  Prefix _ _ _ -> all isPrefix os
  Postfix _ _ _ -> all isPostfix os
  Closed _ _ _ -> all isClosed os

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

data Part = First Bool [String] Kind -- possible successor parts, non-empty
          | Last Bool [String] Bool -- possible predecessor parts, non-empty
          | Lone Opening Bool
          | Middle [String] [String] Kind -- possible predecessor and successor parts, non-empty
  deriving (Show, Eq)

isLone (Lone _ _) = True
isLone _ = False

isFirst (Lone _ _) = True
isFirst (First _ _ _) = True
isFirst _ = False

isLast (Lone _ _) = True
isLast (Last _ _ _) = True
isLast _ = False

isMiddle (Middle _ _ _) = True
isMiddle _ = False

previous (First _ _ _) = []
previous (Last _ l _) = l
previous (Lone _ _) = []
previous (Middle l _ _) = l

data Opening = LeftOpen
             | RightOpen
             | BothOpen
  deriving (Show, Eq)

leftHole :: Part -> Bool
leftHole (First True _ _) = True
leftHole (First _ _ _) = False
leftHole (Last _ _ _) = True
leftHole (Lone RightOpen _) = False
leftHole (Lone _ _) = True
leftHole (Middle _ _ _) = True

rightHole :: Part -> Bool
rightHole (First _ _ _) = True
rightHole (Last True _ _) = True
rightHole (Last _ _ _) = False
rightHole (Lone LeftOpen _) = False
rightHole (Lone _ _) = True
rightHole (Middle _ _ _) = True

rightHoleKind :: Part -> Maybe Kind
rightHoleKind (First _ _ k) = Just k
rightHoleKind (Last _ _ _) = Nothing
rightHoleKind (Lone _ _) = Nothing
rightHoleKind (Middle _ _ k) = Just k

nextPart (First _ r _) = r
nextPart (Last _ _ _) = []
nextPart (Lone _ _) = []
nextPart (Middle _ r _) = r

continue :: String -> Part -> Bool
continue t p = t `elem` nextPart p

part :: Op -> Part

part (Infix [] _ _ _) = error "part called on malformed infix operator"
part (Infix [_] [] _ _) = Lone BothOpen True
part (Infix [_] (r:_) _ _) = First True [r] Distfix
part (Infix l [] _ _) = Last True [last $ init l] True
part (Infix l (r:_) _ _) = Middle [last $ init l] [r] Distfix

part (Prefix [] _ _) = error "part called on malformed prefix operator"
part (Prefix [_] [] _) = Lone RightOpen True
part (Prefix [_] (r:_) _) = First False [r] Distfix
part (Prefix l [] _) = Last True [last $ init l] True
part (Prefix l (r:_) _) = Middle [last $ init l] [r] Distfix

part (Postfix [] _ _) = error "part called on malformed postfix operator"
part (Postfix [_] [] _) = Lone LeftOpen True
part (Postfix [_] (r:_) _) = First True [r] Distfix
part (Postfix l [] _) = Last False [last $ init l] True
part (Postfix l (r:_) _) = Middle [last $ init l] [r] Distfix

part (Closed [] _ _) = error "part called on malformed closed operator"
part (Closed [_] [] _) = error "part called on malformed closed operator"
part (Closed [_] (r:_) SExpression) = First False [r] SExpression
part (Closed [_] (r:_) _) = First False [r] Distfix
part (Closed l [] Discard) = Last False [last $ init l] False
part (Closed l [] DistfixAndDiscard) = Last False [last $ init l] False
part (Closed l [] _) = Last False [last $ init l] True
part (Closed l (r:_) SExpression) = Middle [last $ init l] [r] SExpression
part (Closed l (r:_) _) = Middle [last $ init l] [r] Distfix

parts :: Op -> ([String], [String])
parts (Infix l r _ _) = (l,r)
parts (Prefix l r _) = (l,r)
parts (Postfix l r _) = (l,r)
parts (Closed l r _) = (l,r)

