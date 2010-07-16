module Text.Syntactical.Data (
  SExpr(..), Tree(..), Op(..), Opening(..),
  Associativity(..), Hole(..), Part(..), Table, Priority(..),
  infx, prefx, postfx, closed,
  infx_, prefx_, postfx_, closed_,
  sexpr, distfix,
  buildTable, cut, setPrecedence,
  begin, end, leftOpen, rightOpen, rightHole, discard,
  applicator, applicator', continue, original, priority,
  arity, symbol, symbols, next, previous, current,
  findBoth, findBegin, FindBegin(..),
  Token, toString, operator, consider,
  showPart, showSExpr, showTree
  ) where

import Data.List

----------------------------------------------------------------------
-- Data structures to represent trees, operators, and parts
----------------------------------------------------------------------

-- | The s-expression data type used as input and output of the parser.
data SExpr a = List [SExpr a]
             | Atom a
  deriving (Eq, Show)

-- | The s-expression data type augmented to represent parts (used in
-- the operator stack).
data Tree a = Branch [Tree a]
            | Leaf a
            | Part (Part a)
  deriving (Eq, Show)

-- | The class of the types that can be parsed.
class Token a where
  toString :: a -> String
  -- ^ convert to a string (for showing purpose)
  operator :: Op a -> [SExpr a] -> SExpr a
  -- ^ create an output node from an operator and its arguments
  consider :: a -> a -> Bool
  -- ^ test if two tokens are the same (used to find match from the
  -- operator table)

  -- default definition for consider tests the string representation
  consider a b = toString a == toString b

considers :: Token a => [a] -> [a] -> Bool
considers a b = length a == length b && and (zipWith consider a b)

-- | The operator representation. It allows infix, prefix, postfix,
-- and closed operators, with possibly multiple internal holes.
-- Different holes are possible, to drive the parse in specific ways.
-- The boolean is used to specify if the operator should show up
-- in the result or be discarded. The opening further specifies
-- in the non-closed variant if the operator is prefix, infix, or postfix.
data Op a =
    Op1 Bool a [(Hole,a)] Opening Precedence
  | Op2 Bool a [(Hole,a)] Hole a
  deriving (Eq, Show)

setPrecedence :: Precedence -> Op a -> Op a
setPrecedence p (Op1 keep x xs opening _) = Op1 keep x xs opening p
setPrecedence _ c = c

symbols :: Op a -> [a]
symbols (Op1 _ a xs _ _) = a : map snd xs
symbols (Op2 _ a xs _ b) = a : map snd xs ++ [b]

-- ^ Separate an operator in its different parts.
cut :: Op a -> [Part a]
cut o@(Op1 keep x [] opening p) =
  [Lone o opening p x keep]
cut o@(Op1 _ x xs opening p) =
  First ma x [snd $ head xs] (fst $ head xs) :
  map f (zip4 ls ss rs ks) ++
  [Last o mb (x:(map snd $ init xs)) (snd $ last xs) True ar]
  where
    (ma,mb,ar) = case opening of
      Postfix -> (Just (NonAssociative,p), Nothing, length xs + 1)
      Infix a -> (Just (a,p), Just (a,p), length xs + 2)
      Prefix -> (Nothing, Just (NonAssociative,p), length xs + 1)
    f (l, s, r, k) = Middle l s r k
    (_, xs') = holesAfter (init xs) (fst $ last xs)
    fxs = inits $ map fst xs'
    sxs = inits $ map snd xs'
    ls = map (x:) (init fxs)
    ss = map head (tail fxs)
    rs = map ((++[snd $ last xs]) . tail) (tail fxs)
    ks = map head (tail sxs)

cut o@(Op2 keep x [] h y) =
  [First Nothing x [y] h, Last o Nothing [x] y keep 1]
cut o@(Op2 keep x xs h y) =
  First Nothing x [snd $ head xs] (fst $ head xs) :
  map f (zip4 ls ss rs ks) ++
  [Last o Nothing (x:map snd xs) y keep ar]
  where
    f (l, s, r, k) = Middle l s r k
    (_, xs') = holesAfter xs h
    fxs = inits $ map fst xs'
    sxs = inits $ map snd xs'
    ls = map (x:) (init fxs)
    ss = map head (tail fxs)
    rs = map ((++[y]) . tail) (tail fxs)
    ks = map head (tail sxs)
    ar = length xs + 1

-- Takes a list of pair (hole,string) and returns
-- a list of (string,hole) where the ordre and interleaving
-- is respected: the first hole is returned and the last hole
-- is an argument.
holesAfter :: [(Hole,s)] -> Hole -> (Hole, [(s,Hole)])
holesAfter [] h = (h, [])
holesAfter [(a,b)] h = (a, [(b,h)])
holesAfter ((a,b):xs@((c,_):_)) h = (a, (b,c) : snd (holesAfter xs h))

-- buildTable constructs an operator table that can be
-- used with the shunt function. Operators are given
-- in decreasing precedence order.
buildTable :: [[Op a]] -> Table a
buildTable ls = Table . concat $ zipWith f ls [n, n - 1 .. 0]
  where n = length ls
        f l p = concatMap (cut . setPrecedence p) l

-- | The Hole is used to give various behaviours when dealing
-- with internal holes.
-- SExpression means the 'content' of the hole should be
-- parsed as an s-expression.
-- Distfix means the 'content' of the hole should be parsed
-- as a distfix expression.
data Hole = SExpression | Distfix
  deriving (Eq, Show)

data Associativity = NonAssociative | LeftAssociative | RightAssociative
  deriving (Show, Eq)

type Precedence = Int

data Priority = Lower | Higher | NoPriority

newtype Table a = Table [Part a]

-- NoBegin: no parts with the requested symbol.
-- Begin: found a begin part.
-- MissingBegin: no begin part found but continuing part found.
data FindBegin a = NoBegin | Begin (Part a) | MissingBegin [[a]]

findParts :: Token a => Table a -> a -> [Part a]
findParts (Table ps) x = filter ((consider x) . symbol) ps

findContinuing :: Token a => [Part a] -> Part a -> Maybe (Part a)
findContinuing xs y = case as of
  [] -> Nothing
  (a:_) -> Just $ if isLast a then groupLast as else groupMiddle as
  where as = filter (`continue` y) xs

-- Search the operator stack for the top-most parts waiting to be completed
-- (i.e. on the left of an innner hole).
findIncompletePart :: Table a -> [Tree a] -> Maybe (Part a)
findIncompletePart _ [] = Nothing
findIncompletePart _ (Part y:_) | not (end y) = Just y
findIncompletePart table (_:ss) = findIncompletePart table ss

-- - The operator doesn't contain any operator
-- -> returns (First,Nothing)
-- - The operator stack has an operator at its top and
-- no incomplete operator
-- -> returns (First,Just Top)
-- - The operator stack has no operator at its top but
-- has an incomplete operator below
-- -> return (Continuing of First, Nothing)
-- - The operator stack has an operator at its top and
-- an incomplete operator (at the top or below)
-- -> returns (Continuing or First, Just Top)
-- Actually, if there is no Continuing, returns what it can
-- find, even if it is not First; one of the rules will
-- generate a MissingBefore (in the [] case) or an Incomplete
-- (in the pts2 case).
findBoth :: Token a => Table a -> a -> [Tree a] -> Either (Part a) (FindBegin a)
findBoth table x st = case findIncompletePart table st of
  Nothing -> Right $ findBegin table x
  Just y -> case findContinuing xs y of
    Just a -> Left a
    Nothing -> Right $ findBegin table x
  where xs = findParts table x

findBegin :: Token a => Table a -> a -> FindBegin a
findBegin table x = case filterParts $ findParts table x of
  ([],[],[],[]) -> NoBegin
  ((_:_),(_:_),_,_) -> error "findBegin: ambiguous: lone or first part"
  -- TODO the ambiguity is present when they share the same prefix
  --(_,_,(_:_),(_:_)) -> error "findBegin: ambiguous: middle or last part"
  (l@(_:_),_,_,_) -> Begin $ groupLone l
  (_,f@(_:_),_,_) -> Begin $ groupFirst f
  (_,_,m,l) -> MissingBegin $ map previous (m++l)

-- TODO the complete Op inside then Last and Lone Part makes the other fields
-- unnecessary.

-- | A Part represent a single symbol of an operator.
data Part a = First (Maybe (Associativity,Precedence)) a [a] Hole
-- assoc/prec if it is open, possible successor parts, non-empty, s-expr/distfix
          | Last (Op a) (Maybe (Associativity,Precedence)) [a] a Bool Int
-- assoc/prec if it is open, possible predecessor parts, non-empty, keep/discard, arity
          | Lone (Op a) Opening Precedence a Bool
-- opening, precedence, keep/discard
          | Middle [a] a [a] Hole
-- possible predecessor and successor parts, both non-empty, s-expr/distfix
  deriving (Show, Eq)

-- Specify if an Op1 or a Lone is prefix, postfix, or infix.
data Opening = Infix Associativity
             | Prefix
             | Postfix
  deriving (Show, Eq)

original :: Part a -> Op a
original (Lone o _ _ _ _) = o
original (Last o _ _ _ _ _) = o
original _ = error "can't happen"

priority :: Part a -> Part a -> Priority
priority pt1 pt2 = case (associativity pt1, associativity pt2) of
  (Just (a1,p1), Just (a2,p2)) | begin pt1 && end pt2 ->
    f a1 p1 a2 p2
  _ | isMiddle pt1 || end pt1 && not (isLone pt1) -> Lower
    | otherwise -> Higher
  where f a1 p1 a2 p2
          | p1 == p2 && (a1 == NonAssociative
            || a2 == NonAssociative || a1 /= a2) =
            NoPriority
          | p1 < p2 = Lower
          | p1 == p2 && a1 == LeftAssociative = Lower
          | otherwise = Higher

applicator :: Token a => Table a -> SExpr a -> Bool
applicator table (Atom x) = null $ findParts table x
applicator _ (List _) = True

applicator' :: Token a => Table a -> Tree a -> Bool
applicator' table (Leaf x) = null $ findParts table x
applicator' _ (Branch _) = True
applicator' _ _ = False

isLone :: Part a -> Bool
isLone (Lone _ _ _ _ _) = True
isLone _ = False

isFirst :: Part a -> Bool
isFirst (First _ _ _ _) = True
isFirst _ = False

isLast :: Part a -> Bool
isLast (Last _ _ _ _ _ _) = True
isLast _ = False

isMiddle :: Part a -> Bool
isMiddle (Middle _ _ _ _) = True
isMiddle _ = False

begin :: Part a -> Bool
begin (Lone _ _ _ _ _) = True
begin (First _ _ _ _) = True
begin _ = False

end :: Part a -> Bool
end (Lone _ _ _ _ _) = True
end (Last _ _ _ _ _ _) = True
end _ = False

discard :: Part a -> Bool
discard (First _ _ _ _) = False
discard (Last _ _ _ _ keep _) = not keep
discard (Lone _ _ _ _ keep) = not keep
discard (Middle _ _ _ _) = False

symbol :: Part a -> a
symbol (First _ s _ _) = s
symbol (Last _ _ _ s _ _) = s
symbol (Lone _ _ _ s _) = s
symbol (Middle _ s _ _) = s

arity :: Part a -> Int
arity (First _ _ _ _) = error "arity: bad argument"
arity (Middle _ _ _ _) = error "arity: bad argument"
arity (Lone _ (Infix _) _ _ _) = 2
arity (Lone _ _ _ _ _) = 1
arity (Last _ _ _ _ _ ar) = ar

leftOpen :: Part a -> Bool
leftOpen (First (Just _) _ _ _) = True
leftOpen (First _ _ _ _) = False
leftOpen (Last _ _ _ _ _ _) = True
leftOpen (Lone _ Prefix _ _ _) = False
leftOpen (Lone _ _ _ _ _) = True
leftOpen (Middle _ _ _ _) = True

rightOpen :: Part a -> Bool
rightOpen (First _ _ _ _) = True
rightOpen (Last _ (Just _) _ _ _ _) = True
rightOpen (Last _ _ _ _ _ _) = False
rightOpen (Lone _ Postfix _ _ _) = False
rightOpen (Lone _ _ _ _ _) = True
rightOpen (Middle _ _ _ _) = True

rightHole :: Part a -> Maybe Hole
rightHole (First _ _ _ k) = Just k
rightHole (Last _ _ _ _ _ _) = Nothing
rightHole (Lone _ _ _ _ _) = Nothing
rightHole (Middle _ _ _ k) = Just k

associativity :: Part a -> Maybe (Associativity,Precedence)
associativity (First ap _ _ _) = ap
associativity (Last _ ap _ _ _ _) = ap
associativity (Lone _ Postfix p _ _) = Just (NonAssociative,p)
associativity (Lone _ Prefix p _ _) = Just (NonAssociative,p)
associativity (Lone _ (Infix a) p _ _) = Just (a,p)
associativity (Middle _ _ _ _) = Nothing

next :: Part a -> [a]
next (First _ _ r _) = r
next (Last _ _ _ _ _ _) = []
next (Lone _ _ _ _ _) = []
next (Middle _ _ r _) = r

previous :: Part a -> [a]
previous (First _ _ _ _) = []
previous (Last _ _ l _ _ _) = l
previous (Lone _ _ _ _ _) = []
previous (Middle l _ _ _) = l

current :: Part a -> [a]
current (First _ s _ _) = [s]
current (Last _ _ l s _ _) = l ++ [s]
current (Lone _ _ _ s _) = [s]
current (Middle l s _ _) = l ++ [s]

continue :: Token a => Part a -> Part a -> Bool
continue x y = considers (previous x) (current y)

filterParts :: [Part a] -> ([Part a],[Part a],[Part a],[Part a])
filterParts pts = (filter isLone pts, filter isFirst pts,
  filter isMiddle pts, filter isLast pts)

groupLone :: [Part a] -> Part a
groupLone [pt] = pt
groupLone [] = error "groupLone: empty list"
groupLone _ = error "groupLone: ambiguous lone part, only one allowed"

groupFirst :: Token a => [Part a] -> Part a
groupFirst [] = error "groupFirst: empty list"
groupFirst (First a' x s' k':pts) = go a' s' k' pts
  where go a s k [] = First a x s k
        go a s k (First a2 _ s2 k2:xs)
          | a == a2 && k == k2 = go a (unionBy consider s s2) k xs
        go _ _ _ _ = error "groupFirst: ambiguous first parts"
groupFirst _ = error "groupFirst: not a First part"

groupMiddle :: Token a => [Part a] -> Part a
groupMiddle [] = error "groupMiddle: empty list"
groupMiddle (Middle ss' x s' k':pts) = go ss' s' k' pts
  where go ss s k [] = Middle ss x s k
        go ss s k (Middle ss2 _ s2 k2:xs)
          | not (considers ss ss2) = error "groupMiddle: different prefix"
          | k == k2 = go ss (unionBy consider s s2) k xs
        go _ _ _ _ = error "groupMiddle: ambiguous middle parts"
groupMiddle _ = error "groupMiddle: not a Middle part"

groupLast :: [Part a] -> Part a
groupLast [] = error "groupLast: empty list"
groupLast [l@(Last _ _ _ _ _ _)] = l
groupLast _ = error "groupLast: not a Last part"

----------------------------------------------------------------------
-- Combinators to construct the operator table
----------------------------------------------------------------------

infx :: Associativity -> a -> Op a
infx a f = Op1 True f [] (Infix a) 0

infx_ :: Associativity -> a -> Op a
infx_ a f = Op1 False f [] (Infix a) 0

prefx :: a -> Op a
prefx f = Op1 True f [] Prefix 0

prefx_ :: a -> Op a
prefx_ f = Op1 False f [] Prefix 0

postfx :: a -> Op a
postfx f = Op1 True f [] Postfix 0

postfx_ :: a -> Op a
postfx_ f = Op1 False f [] Postfix 0

closed :: a -> Hole -> a -> Op a
closed f k l = Op2 True f [] k l

closed_ :: a -> Hole -> a -> Op a
closed_ f k l = Op2 False f [] k l

sexpr :: Op a -> a -> Op a
sexpr (Op1 keep x rest opening p) y =
  Op1 keep x (rest++[(SExpression,y)]) opening p
sexpr (Op2 keep x rest k y) z =
  Op2 keep x (rest++[(k,y)]) SExpression z

distfix :: Op a -> a -> Op a
distfix (Op1 keep x rest opening p) y =
  Op1 keep x (rest++[(Distfix,y)]) opening p
distfix (Op2 keep x rest k y) z =
  Op2 keep x (rest++[(k,y)]) Distfix z

----------------------------------------------------------------------
-- A few 'show' functions for SExpr, and Tree
----------------------------------------------------------------------

showSExpr :: Token a => SExpr a -> String
showSExpr = tail . f
  where
  f (Atom s) = ' ' : toString s
  f (List []) = ' ' : "⟨⟩"
  f (List es) = ' ' : '⟨' : tail (concatMap f es) ++ "⟩"

showTree :: Token a => Tree a -> String
showTree = tail . f
  where
  f (Leaf s) = ' ' : toString s
  f (Part y) = ' ' : concatMap toString (current y)
  f (Branch []) = ' ' : "⟨⟩"
  f (Branch es) = ' ' : '⟨' : tail (concatMap f es) ++ "⟩"

showPart :: Token a => Part a -> String
showPart = toString . symbol

