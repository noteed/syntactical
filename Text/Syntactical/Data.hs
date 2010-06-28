module Text.Syntactical.Data where

import Data.List
import Data.Maybe (fromJust)

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
  display' (Op l) = ' ' : concat l
  display' (Node es) = ' ' : '⟨' : tail (concatMap display' es) ++ "⟩"

isSym :: Tree -> Bool
isSym (Sym _) = True
isSym _ = False

isOp :: Tree -> Bool
isOp (Op _) = True
isOp _ = False

isInfix :: Op -> Bool
isInfix (Op1 _ _ _ (BothOpen _) _ _) = True
isInfix _ = False

isPrefix :: Op -> Bool
isPrefix (Op1 _ _ _ (LeftOpen _) _ _) = True
isPrefix _ = False

isPostfix :: Op -> Bool
isPostfix (Op1 _ _ _ (RightOpen _) _ _) = True
isPostfix _ = False

isClosed :: Op -> Bool
isClosed (Op2 _ _ _ _ _) = True
isClosed _ = False

lower :: Part -> Part -> Bool
lower pt1 pt2 = case (associativity pt1, associativity pt2) of
  (Just (a1,p1), Just (a2,p2)) | isFirst pt1 && isLast pt2 ->
    f a1 p1 a2 p2
  _ | isMiddle pt1 || isLast pt1 && not (isLone pt1) -> True
    | otherwise -> False
  where f a1 p1 a2 p2
          | a1 == NonAssociative && a2 == NonAssociative = error "cannot mix"
          | a1 == LeftAssociative && p1 <= p2 = True
          | a1 == RightAssociative && p1 < p2 = True
          | a1 == NonAssociative && p1 <= p2 = True
          | otherwise = False

findPart :: Table -> String -> Maybe Part
findPart table x = case findOp' x table of
  [pt] -> Just pt
  [] -> Nothing

findOp' :: String -> Table -> [Part]
findOp' _ (Table []) = []
findOp' op (Table (o:os)) =
  if op `elem` parts o
  then part' op o : findOp' op (Table os)
  else findOp' op (Table os)

findPart' :: Table -> [String] -> Maybe Part
findPart' table xs = case findPart'' table xs of
  [pt] -> Just pt
  [] -> Nothing

findPart'' (Table []) _ = []
findPart'' (Table (o:os)) xs =
  if xs `isPrefixOf` parts o
  then part' (last xs) o : findPart'' (Table os) xs
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
applicator table (Sym x) = findPart table x == Nothing
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
findParts :: Table -> String -> [String] -> (Part, Part)
findParts table x y = (fromJust $ findPart table x, fromJust $ findPart' table y)
{-
findParts table x y =
  if nonAmbiguous xs && nonAmbiguous ys
  then if null xy then (part $ head xs, part $ head ys) else head xy
  else if nonAmbiguous xs then error $ "ambiguous operators " ++ show ys
                     else error $ "ambiguous operators " ++ show xs
  where xs = findOp x table
        ys = findOps y table
        xy = [(part a, part b) | a <- xs, b <- ys, x `continue` (part b)]
-}

-- TODO check precedence/associativity
nonAmbiguous :: [Op] -> Bool
nonAmbiguous [] = True
nonAmbiguous (o:os)
  | isInfix o = all isInfix os
  | isPrefix o = all isPrefix os
  | isPostfix o = all isPostfix os
  | isClosed o = all isClosed os

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

data Part = First (Maybe (Associativity,Precedence)) [String] Kind
-- assoc/prec if it is open, possible successor parts, non-empty, s-expr/distfix
          | Last (Maybe (Associativity,Precedence)) [String] Bool
-- assoc/prec if it is open, possible predecessor parts, non-empty, keep/discard
          | Lone Opening Precedence Bool
-- opening, precedence, keep/discard
          | Middle [String] [String] Kind
-- possible predecessor and successor parts, both non-empty, s-expr/distfix
  deriving (Show, Eq)

isLone :: Part -> Bool
isLone (Lone _ _ _) = True
isLone _ = False

isFirst :: Part -> Bool
isFirst (Lone _ _ _) = True
isFirst (First _ _ _) = True
isFirst _ = False

isLast :: Part -> Bool
isLast (Lone _ _ _) = True
isLast (Last _ _ _) = True
isLast _ = False

isMiddle :: Part -> Bool
isMiddle (Middle _ _ _) = True
isMiddle _ = False

discard :: Part -> Bool
discard (First _ _ _) = False
discard (Last _ _ keep) = not keep
discard (Lone _ _ keep) = not keep
discard (Middle _ _ _) = False

data Opening = LeftOpen Bool
             | RightOpen Bool
             | BothOpen Associativity
  deriving (Show, Eq)

leftHole :: Part -> Bool
leftHole (First (Just _) _ _) = True
leftHole (First _ _ _) = False
leftHole (Last _ _ _) = True
leftHole (Lone (RightOpen _) _ _) = False
leftHole (Lone _ _ _) = True
leftHole (Middle _ _ _) = True

rightHole :: Part -> Bool
rightHole (First _ _ _) = True
rightHole (Last (Just _) _ _) = True
rightHole (Last _ _ _) = False
rightHole (Lone (LeftOpen _) _ _) = False
rightHole (Lone _ _ _) = True
rightHole (Middle _ _ _) = True

rightHoleKind :: Part -> Maybe Kind
rightHoleKind (First _ _ k) = Just k
rightHoleKind (Last _ _ _) = Nothing
rightHoleKind (Lone _ _ _) = Nothing
rightHoleKind (Middle _ _ k) = Just k

associativity :: Part -> Maybe (Associativity,Precedence)
associativity (First ap _ _) = ap
associativity (Last ap _ _) = ap
associativity (Lone (LeftOpen a) p _) = Just (a',p)
  where a' = if a then LeftAssociative else NonAssociative
associativity (Lone (RightOpen a) p _) = Just (a',p)
  where a' = if a then RightAssociative else NonAssociative
associativity (Lone (BothOpen a) p _) = Just (a,p)
associativity (Middle _ _ _) = Nothing

nextPart :: Part -> [String]
nextPart (First _ r _) = r
nextPart (Last _ _ _) = []
nextPart (Lone _ _ _) = []
nextPart (Middle _ r _) = r

previousPart :: Part -> [String]
previousPart (First _ _ _) = []
previousPart (Last _ l _) = l
previousPart (Lone _ _ _) = []
previousPart (Middle l _ _) = l

continue :: String -> Part -> Bool
continue t p = t `elem` nextPart p

part' :: String -> Op -> Part
-- TODO actually, multiple possibilities exist
part' s (Op1 keep x [] opening _ p) | s == x = Lone opening p keep
part' s (Op1 _ x xs (LeftOpen _) a p) | s == x = First (Just (a,p)) [snd $ head xs] Distfix
part' s (Op1 _ x xs (BothOpen _) a p) | s == x = First (Just (a,p)) [snd $ head xs] Distfix
part' s (Op1 _ x xs (RightOpen _) _ _) | s == x = First Nothing [snd $ head xs] Distfix
part' s (Op1 _ x ([x']) (RightOpen _) a p) | s == snd x' = Last (Just (a,p)) [x] True
part' s (Op1 _ x ([x']) (BothOpen _) a p) | s == snd x' = Last (Just (a,p)) [x] True
part' s (Op1 _ x ([x']) (LeftOpen _) _ _) | s == snd x' = Last Nothing [x] True
part' s (Op1 _ _ xs (RightOpen _) a p) | s == snd (last xs) = Last (Just (a,p)) [snd . last $ init xs] True
part' s (Op1 _ _ xs (BothOpen _) a p) | s == snd (last xs) = Last (Just (a,p)) [snd . last $ init xs] True
part' s (Op1 _ _ xs (LeftOpen _) _ _) | s == snd (last xs) = Last Nothing [snd . last $ init xs] True
part' s (Op1 _ x xs _ _ _) | s == snd (head xs) = Middle [x] [snd . head . drop 1 $ xs] Distfix
part' s (Op1 _ _ xs _ _ _) | s `elem` (map snd $ init xs) = Middle [last . takeWhile (s /=) $ map snd xs] [head . drop 1 . dropWhile (s /=) $ map snd xs] Distfix

part' s (Op2 _ x [] h y) | s == x = First Nothing [y] h
part' s (Op2 keep x [] _ y) | s == y = Last Nothing [x] keep
part' s (Op2 _ x xs h _) | s == x = First Nothing [snd $ head xs] h
part' s (Op2 keep _ xs _ y) | s == y = Last Nothing [snd $ last xs] keep
part' s (Op2 _ x [x'] h y) | s == (snd x') = Middle [x] [y] h
part' s (Op2 _ x xs h _) | s == (snd $ head xs) = Middle [x] [snd . head . drop 1 $ xs] h
part' s (Op2 _ _ xs h y) | s == (snd $ last xs) = Middle [snd . last . init $ xs] [y] h
part' s (Op2 _ _ xs h _) | s `elem` (map snd $ init xs) = Middle [last . takeWhile (s /=) $ map snd xs] [head . drop 1 . dropWhile (s /=) $ map snd xs] h

parts :: Op -> [String]
parts (Op1 _ x xs _ _ _) = x : map snd xs
parts (Op2 _ x xs _ y) = x : map snd xs ++ [y]

