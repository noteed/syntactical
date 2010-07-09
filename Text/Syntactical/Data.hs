module Text.Syntactical.Data (
  Token, operator,
  Tree(..), Op(..), Opening(..), Associativity(..), Kind(..), Table,
  infx, prefx, postfx, closed, closed_,
  buildTable,
  begin, end, leftHole, rightHole, rightHoleKind, discard,
  applicator, continue, lower,
  arity, partSymbol, nextPart, previousPart,
  findBoth, findBegin, FindBegin(..)
  ) where

import Data.List
--import qualified Data.Map as M

class Eq a => Token a where
  operator :: [a] -> a
  -- ^ Create a new operator token from a list of part tokens.

-- The s-expression data type, abstracting over the token type.
data Tree a = Node [Tree a]
           | Sym a
           | Part (Part a) -- on the stack, TODO turn into Sym on the output
  deriving (Eq, Show)

-- The boolean is to specify if the operator should show up
-- in the result or be discarded. The opening further specifies
-- if the operator is prefix, infix, or postfix.
-- e.g. ⟨1 2⟩ instead of ⟨+ 1 2⟩ when the + is retained.
-- e.g. 1 instead of ⟨! 1⟩ when the ! is retained.
-- e.g. ⟨1⟩ instead of ⟨! 1⟩ if the hole is also parsed as an s-expr.
data Op a =
    Op1 Bool a [(Kind,a)] Opening Associativity Precedence
  | Op2 Bool a [(Kind,a)] Kind a
  deriving (Eq, Show)

-- The Kind is used to give various behaviours when dealing
-- with internal holes. Maybe it could be extended to
-- support user-defined parsers, ot at least a specific
-- operator table.
-- SExpression means the 'content' of the hole should be
-- parsed as an s-expression.
-- Distfix means the 'content' of the hole should be parsed
-- as a distfix expression.
data Kind = SExpression | Distfix
  deriving (Eq, Show)

data Associativity = NonAssociative | LeftAssociative | RightAssociative
  deriving (Show, Eq)

type Precedence = Int

newtype Table a = Table [Part a]

infx :: a -> [a] -> Associativity -> Op a
-- TODO the associativity is present twice
infx f rest a = Op1 True f (zip (repeat Distfix) rest) (BothOpen a) a 0

prefx :: a -> [a] -> Op a
prefx f rest = Op1 True f (zip (repeat Distfix) rest) (RightOpen True) RightAssociative 0

postfx :: a -> [a] -> Op a
postfx f rest = Op1 True f (zip (repeat Distfix) rest) (LeftOpen True) RightAssociative 0

closed :: a -> [a] -> a -> Kind -> Op a
closed f rest l k = Op2 True f (zip (repeat k) rest) k l

closed_ :: a -> [a] -> a -> Kind -> Op a
closed_ f rest l k = Op2 False f (zip (repeat k) rest) k l

setPrecedence :: Precedence -> Op a -> Op a
setPrecedence p (Op1 keep x xs opening a _) = Op1 keep x xs opening a p
setPrecedence _ c = c

-- buildTable constructs an operator table that can be
-- used with the shunt function. Operators are given
-- in decreasing precedence order.
-- TODO see if Parsec's buildExpressionParser uses a
-- incresing or decreasing list.
buildTable :: [[Op a]] -> Table a
buildTable ls = Table . concat $ zipWith f ls [n, n - 1 .. 0]
  where n = length ls
        f l p = concatMap (cut . setPrecedence p) l

lower :: Part a -> Part a -> Bool
lower pt1 pt2 = case (associativity pt1, associativity pt2) of
  (Just (a1,p1), Just (a2,p2)) | begin pt1 && end pt2 ->
    f a1 p1 a2 p2
  _ | isMiddle pt1 || end pt1 && not (isLone pt1) -> True
    | otherwise -> False
  where f a1 p1 a2 p2
          | a1 == NonAssociative && a2 == NonAssociative = error "cannot mix"
          | a1 == LeftAssociative && p1 <= p2 = True
          | a1 == RightAssociative && p1 < p2 = True
          | a1 == NonAssociative && p1 <= p2 = True
          | otherwise = False

findParts :: Token a => Table a -> a -> [Part a]
findParts (Table ps) x = filter ((==x) . partSymbol) ps

applicator :: Token a => Table a -> Tree a -> Bool
applicator table (Sym x) = findParts table x == []
applicator _ (Node _) = True
applicator _ _ = False

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

filterParts :: [Part a] -> ([Part a],[Part a],[Part a],[Part a])
filterParts pts = (filter isLone pts, filter isFirst pts,
  filter isMiddle pts, filter isLast pts)

-- NoBegin: no parts with the requested symbol.
-- Begin: found a begin part.
-- MissingBegin: no begin part found but continuing part found.
data FindBegin a = NoBegin | Begin (Part a) | MissingBegin [[a]]

findBegin :: Token a => Table a -> a -> FindBegin a
findBegin table x = case filterParts $ findParts table x of
  ([],[],[],[]) -> NoBegin
  ((_:_),(_:_),_,_) -> error "findBegin: ambiguous: lone or first part"
  -- TODO the ambiguity is present when they share the same prefix
  --(_,_,(_:_),(_:_)) -> error "findBegin: ambiguous: middle or last part"
  (l@(_:_),_,_,_) -> Begin $ groupLone l
  (_,f@(_:_),_,_) -> Begin $ groupFirst f
  (_,_,m,l) -> MissingBegin $ map previousPart (m++l)

groupLone :: [Part a] -> Part a
groupLone [pt] = pt
groupLone [] = error "groupLone: empty list"
groupLone _ = error "groupLone: ambiguous lone part, only one allowed"

groupFirst :: Token a => [Part a] -> Part a
groupFirst [] = error "groupFirst: empty list"
groupFirst (First a' x s' k':pts) = go a' s' k' pts
  where go a s k [] = First a x s k
        go a s k (First a2 _ s2 k2:xs)
          | a == a2 && k == k2 = go a (s `union` s2) k xs
        go _ _ _ _ = error "groupFirst: ambiguous first parts"
groupFirst _ = error "groupFirst: not a First part"

groupMiddle :: Token a => [Part a] -> Part a
groupMiddle [] = error "groupMiddle: empty list"
groupMiddle (Middle ss' x s' k':pts) = go ss' s' k' pts
  where go ss s k [] = Middle ss x s k
        go ss s k (Middle ss2 _ s2 k2:xs)
          | ss /= ss2 = error "groupMiddle: different prefix"
          | k == k2 = go ss (s `union` s2) k xs
        go _ _ _ _ = error "groupMiddle: ambiguous middle parts"
groupMiddle _ = error "groupMiddle: not a Middle part"

groupLast :: [Part a] -> Part a
groupLast [] = error "groupLast: empty list"
groupLast [l@(Last _ _ _ _ _)] = l
groupLast _ = error "groupLast: not a Last part"

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

data Part a = First (Maybe (Associativity,Precedence)) a [a] Kind
-- assoc/prec if it is open, possible successor parts, non-empty, s-expr/distfix
          | Last (Maybe (Associativity,Precedence)) [a] a Bool Int
-- assoc/prec if it is open, possible predecessor parts, non-empty, keep/discard, arity
          | Lone Opening Precedence a Bool
-- opening, precedence, keep/discard
          | Middle [a] a [a] Kind
-- possible predecessor and successor parts, both non-empty, s-expr/distfix
  deriving (Show, Eq)

isLone :: Part a -> Bool
isLone (Lone _ _ _ _) = True
isLone _ = False

isFirst :: Part a -> Bool
isFirst (First _ _ _ _) = True
isFirst _ = False

isLast :: Part a -> Bool
isLast (Last _ _ _ _ _) = True
isLast _ = False

isMiddle :: Part a -> Bool
isMiddle (Middle _ _ _ _) = True
isMiddle _ = False

begin :: Part a -> Bool
begin (Lone _ _ _ _) = True
begin (First _ _ _ _) = True
begin _ = False

end :: Part a -> Bool
end (Lone _ _ _ _) = True
end (Last _ _ _ _ _) = True
end _ = False

discard :: Part a -> Bool
discard (First _ _ _ _) = False
discard (Last _ _ _ keep _) = not keep
discard (Lone _ _ _ keep) = not keep
discard (Middle _ _ _ _) = False

partSymbol :: Part a -> a
partSymbol (First _ s _ _) = s
partSymbol (Last _ _ s _ _) = s
partSymbol (Lone _ _ s _) = s
partSymbol (Middle _ s _ _) = s

arity :: Part a -> Int
arity (First _ _ _ _) = error "arity: bad argument"
arity (Middle _ _ _ _) = error "arity: bad argument"
arity (Lone (BothOpen _) _ _ _) = 2
arity (Lone _ _ _ _) = 1
arity (Last _ _ _ _ ar) = ar

data Opening = LeftOpen Bool
             | RightOpen Bool
             | BothOpen Associativity
  deriving (Show, Eq)

leftHole :: Part a -> Bool
leftHole (First (Just _) _ _ _) = True
leftHole (First _ _ _ _) = False
leftHole (Last _ _ _ _ _) = True
leftHole (Lone (RightOpen _) _ _ _) = False
leftHole (Lone _ _ _ _) = True
leftHole (Middle _ _ _ _) = True

rightHole :: Part a -> Bool
rightHole (First _ _ _ _) = True
rightHole (Last (Just _) _ _ _ _) = True
rightHole (Last _ _ _ _ _) = False
rightHole (Lone (LeftOpen _) _ _ _) = False
rightHole (Lone _ _ _ _) = True
rightHole (Middle _ _ _ _) = True

rightHoleKind :: Part a -> Maybe Kind
rightHoleKind (First _ _ _ k) = Just k
rightHoleKind (Last _ _ _ _ _) = Nothing
rightHoleKind (Lone _ _ _ _) = Nothing
rightHoleKind (Middle _ _ _ k) = Just k

associativity :: Part a -> Maybe (Associativity,Precedence)
associativity (First ap _ _ _) = ap
associativity (Last ap _ _ _ _) = ap
associativity (Lone (LeftOpen a) p _ _) = Just (a',p)
  where a' = if a then LeftAssociative else NonAssociative
associativity (Lone (RightOpen a) p _ _) = Just (a',p)
  where a' = if a then RightAssociative else NonAssociative
associativity (Lone (BothOpen a) p _ _) = Just (a,p)
associativity (Middle _ _ _ _) = Nothing

nextPart :: Part a -> [a]
nextPart (First _ _ r _) = r
nextPart (Last _ _ _ _ _) = []
nextPart (Lone _ _ _ _) = []
nextPart (Middle _ _ r _) = r

previousPart :: Part a -> [a]
previousPart (First _ _ _ _) = []
previousPart (Last _ l _ _ _) = l
previousPart (Lone _ _ _ _) = []
previousPart (Middle l _ _ _) = l

continue :: Token a => Part a -> Part a -> Bool
continue x y = previousPart x == previousPart y ++ [partSymbol y]

cut :: Op a -> [Part a]
cut (Op1 keep x [] opening _ p) =
  [Lone opening p x keep]
cut (Op1 _ x xs opening a p) =
  First ma x [snd $ head xs] (fst $ head xs) :
  map f (zip4 ls ss rs ks) ++
  [Last mb (x:(map snd $ init xs)) (snd $ last xs) True ar]
  where
    j = Just (a,p)
    (ma,mb,ar) = case opening of
      LeftOpen _ -> (j, Nothing, length xs + 1)
      BothOpen _ -> (j, j, length xs + 2)
      RightOpen _ -> (Nothing, j, length xs + 1)
    f (l, s, r, k) = Middle l s r k
    (_, xs') = holesAfter (init xs) (fst $ last xs)
    fxs = inits $ map fst xs'
    sxs = inits $ map snd xs'
    ls = map (x:) (init fxs)
    ss = map head (tail fxs)
    rs = map ((++[snd $ last xs]) . tail) (tail fxs)
    ks = map head (tail sxs)

cut (Op2 keep x [] h y) =
  [First Nothing x [y] h, Last Nothing [x] y keep 1]
cut (Op2 keep x xs h y) =
  First Nothing x [snd $ head xs] h :
  map f (zip4 ls ss rs ks) ++
  [Last Nothing (x:map snd xs) y keep ar]
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
holesAfter :: [(Kind,s)] -> Kind -> (Kind, [(s,Kind)])
holesAfter [] h = (h, [])
holesAfter [(a,b)] h = (a, [(b,h)])
holesAfter ((a,b):xs@((c,_):_)) h = (a, (b,c) : snd (holesAfter xs h))

-- TODO here and for groupXxx, make distinct data type for Lone, First, Middle and Last)
-- partMap :: Table -> (M.Map String Part, M.Map String Part, M.Map String Part, M.Map String Part)
-- partMap (Table ops) =
--  where
--    (lones, fs, ms, ls) = filterParts $ concatMap cut ops

