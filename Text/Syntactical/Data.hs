module Text.Syntactical.Data (
  Tree(..), Op(..), Opening(..), Associativity(..), Kind(..), Table(..),
  findBegin, FindBegin(..),
  infx, prefx, postfx, closed, closed_,
  buildTable,
  arity, partSymbol,
  begin, end, leftHole, rightHole, rightHoleKind, discard,
  applicator, continue, continue', lower,
  nextPart, previousPart,
  findBoth, findOps, findPart, findPart'
  ) where

import Data.List
import Data.Maybe (fromJust)
import qualified Data.Map as M

data Tree = Node [Tree]
-- The to-be-shunted tokens. Only the information for the
-- shunting yard algorithm is represented. Actual tokens should
-- be converted to this representation.
-- TODO if the above is true, then algorithm should return the
-- original data instead of a Tree.
           | Num Int
           | Sym String
           | Op Part -- on the stack, TODO turn into Sym on the output
  deriving Eq

-- The boolean is to specify if the operator should show up
-- in the result or be discarded. The opening further specifies
-- if the operator is prefix, infix, or postfix.
-- e.g. ⟨1 2⟩ instead of ⟨+ 1 2⟩ when the + is retained.
-- e.g. 1 instead of ⟨! 1⟩ when the ! is retained.
-- e.g. ⟨1⟩ instead of ⟨! 1⟩ if the hole is also parsed as an s-expr.
data Op =
    Op1 Bool String [(Kind,String)] Opening Associativity Precedence
  | Op2 Bool String [(Kind,String)] Kind String
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

newtype Table = Table [Op]

infx :: String -> [String] -> Associativity -> Op
-- TODO the associativity is present twice
infx f rest a = Op1 True f (zip (repeat Distfix) rest) (BothOpen a) a 0

prefx :: String -> [String] -> Op
prefx f rest = Op1 True f (zip (repeat Distfix) rest) (RightOpen True) RightAssociative 0

postfx :: String -> [String] -> Op
postfx f rest = Op1 True f (zip (repeat Distfix) rest) (LeftOpen True) RightAssociative 0

closed :: String -> [String] -> String -> Kind -> Op
closed f rest l k = Op2 True f (zip (repeat k) rest) k l

closed_ :: String -> [String] -> String -> Kind -> Op
closed_ f rest l k = Op2 False f (zip (repeat k) rest) k l

setPrecedence :: Precedence -> Op -> Op
setPrecedence p (Op1 keep x xs opening a _) = Op1 keep x xs opening a p
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
  display' (Op y) = ' ' : concat (previousPart y ++ [partSymbol y])
  display' (Node es) = ' ' : '⟨' : tail (concatMap display' es) ++ "⟩"

lower :: Part -> Part -> Bool
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

-- Returns a begin part in priority.
-- TODO findPart is used to find a begin part;
-- if it can't find it, it is an error and it
-- should make available the previous parts needed.
findPart :: Table -> String -> Maybe Part
findPart table x = case filterParts $ findOp' table x of
  ([],[],[],[]) -> Nothing
  ((_:_),(_:_),_,_) -> error "findPart: ambiguous: lone or first part"
  (_,_,(_:_),(_:_)) -> error "findPart: ambiguous: middle or last part"
  (l@(_:_),_,_,_) -> Just $ groupLone l
  (_,f@(_:_),_,_) -> Just $ groupFirst f
  (_,_,m@(_:_),_) -> Just $ groupMiddle m
  (_,_,_,l@(_:_)) -> Just $ groupLast l

findLone table x = if null ls
  then Nothing
  else Just $ groupLone ls
  where
  (ls,_,_,_) = filterParts $ findOp' table x

findFirst table x = if null fs
  then Nothing
  else Just $ groupFirst fs
  where
  (_,fs,_,_) = filterParts $ findOp' table x

findMiddle table x = if null ms
  then Nothing
  else Just $ groupMiddle ms
  where
  (_,_,ms,_) = filterParts $ findOp' table x

findLast table x = if null ls
  then Nothing
  else Just $ groupLast ls
  where
  (_,_,_,ls) = filterParts $ findOp' table x

findOp' :: Table -> String -> [Part]
findOp' (Table []) _ = []
findOp' (Table (o:os)) op =
  if op `elem` parts o
  then part op o ++ findOp' (Table os) op
  else findOp' (Table os) op

findPart' :: Table -> [String] -> Maybe Part
findPart' table xs = case filterParts $ findPart'' table xs of
  ([],[],[],[]) -> Nothing
  ((_:_),(_:_),_,_) -> error "findPart: ambiguous: lone or first part"
  (_,_,(_:_),(_:_)) -> error "findPart: ambiguous: middle or last part"
  (l@(_:_),_,_,_) -> Just $ groupLone l
  (_,f@(_:_),_,_) -> Just $ groupFirst f
  (_,_,m@(_:_),_) -> Just $ groupMiddle m
  (_,_,_,l@(_:_)) -> Just $ groupLast l

findPart'' :: Table -> [String] -> [Part]
findPart'' (Table []) _ = []
findPart'' (Table (o:os)) xs =
  if xs `isPrefixOf` parts o
  then part (last xs) o ++ findPart'' (Table os) xs
  else findPart'' (Table os) xs

findOps :: [String] -> Table -> [Op]
findOps ops (Table t) = findOps' ops t

findOps' :: [String] -> [Op] -> [Op]
findOps' _ [] = []
findOps' ops (o:os) =
  if ops `isPrefixOf` parts o
  then o : findOps' ops os
  else findOps' ops os

applicator :: Table -> Tree -> Bool
applicator table (Sym x) = findOp' table x == []
applicator _ (Node _) = True
applicator _ _ = False

findContinuing :: Table -> String -> Part -> Maybe Part
findContinuing table x y =
  case as of
    [] -> Nothing
    _ -> Just $ if isLast (head as) then groupLast as else groupMiddle as
  where xs = findOp' table x
        as = [a | a <- xs, a `continue'` y]

-- Search the operator stack for the top-most parts waiting to be completed
-- (i.e. on the left of an innner hole).
findIncompletePart :: Table -> [Tree] -> Maybe Part
findIncompletePart _ [] = Nothing
findIncompletePart table (Op y:ss) =
  if (not . end) y
  then Just y
  else findIncompletePart table ss
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
findBoth :: Table -> String -> [Tree] -> (Either Part FindBegin, Maybe Part)
findBoth table x st = case findIncompletePart table st of
  Nothing -> (Right $ findBegin table x, b)
  Just y' -> case findContinuing table x y' of
    Just a -> (Left a, if Op y' == head st then Just y' else b)
    Nothing -> (Right $ findBegin table x, b)
  where
    b = case st of
      (Op y:_) -> Just y
      _ -> Nothing

filterParts :: [Part] -> ([Part],[Part],[Part],[Part])
filterParts pts = (filter isLone pts, filter isFirst pts,
  filter isMiddle pts, filter isLast pts)

data FindBegin = NoBegin | Begin Part | MissingBegin [[String]]
--ContinueTop | ContinueBelow | Begin | Mismatch

findBegin :: Table -> String -> FindBegin
findBegin table x = case filterParts $ findOp' table x of
  ([],[],[],[]) -> NoBegin
  ((_:_),(_:_),_,_) -> error "findBegin: ambiguous: lone or first part"
  (_,_,(_:_),(_:_)) -> error "findBegin: ambiguous: middle or last part"
  (l@(_:_),_,_,_) -> Begin $ groupLone l
  (_,f@(_:_),_,_) -> Begin $ groupFirst f
  (_,_,m,l) -> MissingBegin $ map previousPart (m++l)

allParts :: Table -> [Part]
allParts (Table ops) = concatMap cut ops

matchCurrent x pts = filter ((==x) .partSymbol) pts
matchPrevious y pts = filter ((==y) . previousPart) pts

-- TODO the groupXxx functions should not use 'union' but
-- test if the previous+next parts are the same (and raise
-- an erro if it is the case).

groupLone :: [Part] -> Part
groupLone [pt] = pt
groupLone [] = error "groupLone: empty list"
groupLone _ = error "groupLone: ambiguous lone part, only one allowed"

groupFirst :: [Part] -> Part
groupFirst [] = error "groupFirst: empty list"
groupFirst (First a' x s' k':pts) = go a' s' k' pts
  where go a s k [] = First a x s k
        go a s k (First a2 _ s2 k2:xs)
          | a == a2 && k == k2 = go a (s `union` s2) k xs
        go _ _ _ _ = error "groupFirst: ambiguous first parts"
groupFirst _ = error "groupFirst: not a First part"

-- TODO k == k2 is necessary only when the prefix is the same
groupMiddle :: [Part] -> Part
groupMiddle [] = error "groupMiddle: empty list"
groupMiddle (Middle ss' x s' k':pts) = go ss' s' k' pts
  where go ss s k [] = Middle ss x s k
        go ss s k (Middle ss2 _ s2 k2:xs)
          | ss /= ss2 = error "groupMiddle: different prefix"
        go ss s k (Middle _ _ s2 k2:xs)
          | k == k2 = go ss (s `union` s2) k xs
        go _ _ _ _ = error "groupMiddle: ambiguous middle parts"
groupMiddle _ = error "groupMiddle: not a Middle part"

-- This is needed only to return something so that the
-- shunt can complain about missing parts before this one.
-- Thus, the associativity and keep values do not matter.
-- (In other words, this is used for the union of the previous
-- parts.)
-- TODO x is assumed to be the same (idem for the other groupXxx functions).
groupLast :: [Part] -> Part
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

data Part = First (Maybe (Associativity,Precedence)) String [String] Kind
-- assoc/prec if it is open, possible successor parts, non-empty, s-expr/distfix
          | Last (Maybe (Associativity,Precedence)) [String] String Bool Int
-- assoc/prec if it is open, possible predecessor parts, non-empty, keep/discard, arity
          | Lone Opening Precedence String Bool
-- opening, precedence, keep/discard
          | Middle [String] String [String] Kind
-- possible predecessor and successor parts, both non-empty, s-expr/distfix
  deriving (Show, Eq)

isLone :: Part -> Bool
isLone (Lone _ _ _ _) = True
isLone _ = False

isFirst :: Part -> Bool
isFirst (First _ _ _ _) = True
isFirst _ = False

isLast :: Part -> Bool
isLast (Last _ _ _ _ _) = True
isLast _ = False

isMiddle :: Part -> Bool
isMiddle (Middle _ _ _ _) = True
isMiddle _ = False

begin :: Part -> Bool
begin (Lone _ _ _ _) = True
begin (First _ _ _ _) = True
begin _ = False

end :: Part -> Bool
end (Lone _ _ _ _) = True
end (Last _ _ _ _ _) = True
end _ = False

discard :: Part -> Bool
discard (First _ _ _ _) = False
discard (Last _ _ _ keep _) = not keep
discard (Lone _ _ _ keep) = not keep
discard (Middle _ _ _ _) = False

partSymbol :: Part -> String
partSymbol (First _ s _ _) = s
partSymbol (Last _ _ s _ _) = s
partSymbol (Lone _ _ s _) = s
partSymbol (Middle _ s _ _) = s

arity (First _ _ _ _) = error "arity: bad argument"
arity (Middle _ _ _ _) = error "arity: bad argument"
arity (Lone (BothOpen _) _ _ _) = 2
arity (Lone _ _ _ _) = 1
arity (Last _ p _ _ ar) = ar

data Opening = LeftOpen Bool
             | RightOpen Bool
             | BothOpen Associativity
  deriving (Show, Eq)

leftHole :: Part -> Bool
leftHole (First (Just _) _ _ _) = True
leftHole (First _ _ _ _) = False
leftHole (Last _ _ _ _ _) = True
leftHole (Lone (RightOpen _) _ _ _) = False
leftHole (Lone _ _ _ _) = True
leftHole (Middle _ _ _ _) = True

rightHole :: Part -> Bool
rightHole (First _ _ _ _) = True
rightHole (Last (Just _) _ _ _ _) = True
rightHole (Last _ _ _ _ _) = False
rightHole (Lone (LeftOpen _) _ _ _) = False
rightHole (Lone _ _ _ _) = True
rightHole (Middle _ _ _ _) = True

rightHoleKind :: Part -> Maybe Kind
rightHoleKind (First _ _ _ k) = Just k
rightHoleKind (Last _ _ _ _ _) = Nothing
rightHoleKind (Lone _ _ _ _) = Nothing
rightHoleKind (Middle _ _ _ k) = Just k

associativity :: Part -> Maybe (Associativity,Precedence)
associativity (First ap _ _ _) = ap
associativity (Last ap _ _ _ _) = ap
associativity (Lone (LeftOpen a) p _ _) = Just (a',p)
  where a' = if a then LeftAssociative else NonAssociative
associativity (Lone (RightOpen a) p _ _) = Just (a',p)
  where a' = if a then RightAssociative else NonAssociative
associativity (Lone (BothOpen a) p _ _) = Just (a,p)
associativity (Middle _ _ _ _) = Nothing

nextPart :: Part -> [String]
nextPart (First _ _ r _) = r
nextPart (Last _ _ _ _ _) = []
nextPart (Lone _ _ _ _) = []
nextPart (Middle _ _ r _) = r

previousPart :: Part -> [String]
previousPart (First _ _ _ _) = []
previousPart (Last _ l _ _ _) = l
previousPart (Lone _ _ _ _) = []
previousPart (Middle l _ _ _) = l

continue :: String -> Part -> Bool
continue t p = t `elem` nextPart p

continue' :: Part -> Part -> Bool
continue' x y = previousPart x == previousPart y ++ [partSymbol y]

part :: String -> Op -> [Part]
part s o = filter ((==s) . partSymbol) $ cut o

parts :: Op -> [String]
parts (Op1 _ x xs _ _ _) = x : map snd xs
parts (Op2 _ x xs _ y) = x : map snd xs ++ [y]

cut :: Op -> [Part]
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
holesAfter :: [(Kind,String)] -> Kind -> (Kind, [(String,Kind)])
holesAfter [] h = (h, [])
holesAfter [(a,b)] h = (a, [(b,h)])
holesAfter ((a,b):xs@((c,_):_)) h = (a, (b,c) : snd (holesAfter xs h))

-- TODO here and for groupXxx, make distinct data type for Lone, First, Middle and Last)
-- partMap :: Table -> (M.Map String Part, M.Map String Part, M.Map String Part, M.Map String Part)
-- partMap (Table ops) =
--  where
--    (lones, fs, ms, ls) = filterParts $ concatMap cut ops

