-- 2009.05.05
-- 2009.06.08
-- 2010.05.01
-- The Shunting-Yard algorithm (modified to allow function
-- application without parens around the arguments, and just
-- blanks between arguments).
-- TODO make sure the rules reflect what's going on, a same
-- rule should be associated to a same behavior.
-- TODO the precedence comparison should depends on the
-- associativity even for Pre/Postfix (see 'lower').
-- (TODO a pre/postfix operator can be associative or non-associative.)
-- TODO factorize
-- TODO is ! a + b allowed if ! and + have the same precedence?
-- TODO allow specific operator table for internal operator holes
-- (e.g. to reuse a same symbol with different fixity/precedecence).
-- TODO use specific data types for the elements of each stack. 
-- TODO replace the use of (head . findOp).
-- TODO test more infix ops before a postfix (e.g. a + b * c _/ d /.)
-- to exercice flushHigher.
-- TODO use HPC to see if tests cover the code.
-- TODO maybe feed random tokens to the algorithm to see if it can crash.
-- TODO use hlint.
-- TODO write more realistic example (for a Haskell-like syntax)

module Text.Syntactical.Yard where

import Data.List (intersperse)

import Text.Syntactical.Data

-- An applicator is a non-operator symbol that is applied
-- to some arguments. When such a symbol is read, it is
-- placed on the (operator/applicator) stack. If there is
-- already such a symbol on the stack, it goes straight
-- to the output stack (this is the Inert case).
data Rule = Initial
          | Inert      -- not an operator or an applicator, goes
                       -- straight to the output stack
          | MakeInert  -- apply and push on the the input stack
          | Application -- apply an applicator
          | FlushApp   -- apply an applicator
          | StackApp   -- push an applicator to the stack
          | FlushOp    -- apply an operator
          | StackL     -- push the first part of a closed or prefix operator
          | StackOp    -- push a new operator part to the stack
          | ContinueOp -- append an operator part to the operator
                       -- at the top of the stack
          | MatchedR   -- handle the last part of a closed operator
          | SExpr      -- build an s-expression
          | Done Done
  deriving (Show, Eq)

data Done =
    Success    -- everything is successfuly parsed
  | MissingBefore [String] String -- error case: missing parts before part
  | MissingAfter String [String]  -- error case: missing part before parts
  | CantMix Op Op -- error case: can't mix two operators
  | CantApply Int Int -- error case: can't apply number to number
  | EmptyHole String String -- error case: no sub-expression between ops/parts.
  | Incomplete [String] -- error case: missing operator part(s) after the strings.
  | Unexpected -- unexpected state (can't happen, this is a bug)
  deriving (Eq, Show)

isDone :: Shunt -> Bool
isDone sh = case rule sh of
  Done _ -> True
  _ -> False

showDone :: Done -> [Char]
showDone d = case d of
  Success -> "Parsing successful"
  MissingAfter p ps -> "Parse error: missing operator part " ++
    p ++ " after " ++ concat (intersperse " " ps)
  MissingBefore ps p -> "Parse error: missing operator parts " ++
    concat (intersperse " " ps) ++ " before " ++ p
  CantMix _ _ -> "Parse error: cannot mix operators"
  CantApply a b -> "Parse error: cannot apply " ++ show a ++ " to " ++ show b
  EmptyHole a b -> "Parse error: no sub-expression between " ++ a ++ " and " ++ b
  Incomplete s -> "Parse error: missing operator parts after " ++ show s
  Unexpected -> "Parsing raised a bug"

stackedOp :: Rule -> Bool
stackedOp StackL = True
stackedOp StackOp = True
stackedOp ContinueOp = True
stackedOp _ = False

data Shunt = S {
    input :: [Tree]    -- list of tokens (Nodes can be pushed back.)
  , stack :: [Tree]    -- stack of operators and applicators
  , output :: [[Tree]] -- stack of stacks
  , rule :: Rule
  }

instance Show Shunt where
  show (S ts ss os ru) =
    pad 20 ts ++ pad 20 ss ++ pad 20 os ++ "   " ++ show ru

pad :: Show a => Int -> a -> [Char]
pad n s = let s' = show s in replicate (n - length s') ' ' ++ s'

steps :: Table -> [Tree] -> IO ()
steps table ts = do
  putStrLn $ "               Input               Stack              Output   Rule"
  let sh = iterate (step table) $ initial ts
  let l = length $ takeWhile (not . isDone) sh
  mapM_ (putStrLn . show) (take (l + 1) sh)

initial :: [Tree] -> Shunt
initial ts = S ts [] [[]] Initial

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

shunt :: Table -> [Tree] -> Either Shunt Tree
shunt table ts = case fix $ initial ts of
  S [] [] [[o']] (Done Success) -> Right o'
  s -> Left s
  where fix s = let s' = step table s in
                if sat table s' then if isDone s' then s' else fix s'
                                else error "invariants not satified"

sat :: Table -> Shunt -> Bool
sat table s = all (\i -> i table s) invariants

invariants :: [Table -> Shunt -> Bool]
invariants =
  [ -- inv1
  ]

inv1 :: Table -> Shunt -> Bool
inv1 table (S _ (Op x:ss) oss Inert) =
  case findOps x table of
    [Infix _ _ _ _] -> countInfix table 0 (Op x:ss) + 1 == length (head oss)
    _ -> True
inv1 _ _ = True

countInfix :: Table -> Int -> [Tree] -> Int
countInfix _ acc [] = acc
countInfix table acc (Op x:ss) = case findOps x table of
  [Infix _ _ _ _] -> countInfix table (acc + 1) ss
  _ -> countInfix table acc ss
countInfix table acc (Sym _:ss) = countInfix table acc ss
countInfix table acc (Node _:ss) = countInfix table acc ss
countInfix _ _ (Num _:_) =
  error "can't happen: but TODO make a specific data type for the op stack"

step :: Table -> Shunt -> Shunt

step table (S tt st@(s@(Op y):ss) oo@(os:oss) ru) |
  let o2 = head $ findOps y table
      (_,r2) = parts o2
      end = null r2
  in end && isPostfix o2
  =
  S (o:tt) ss (os':oss') MatchedR
  where ((o:os'):oss') = apply table s oo

step table (S tt st@(s@(Op y):ss) oo@(os:oss) ru) |
  let o2 = head $ findOps y table
      (_,r2) = parts o2
      end = null r2
  in end && isClosed o2
  = case head $ findOps y table of
  Closed _ _ Discard -> S (o:tt) ss (os':oss) MatchedR where (o:os') = os
  Closed _ _ DistfixAndDiscard -> S (o:tt) ss (os':oss) MatchedR where (o:os') = os
  _ -> S (o:tt) ss (os':oss') MatchedR where ((o:os'):oss') = apply table s oo

-- A number is on the input stack. It goes straight
-- to the output unless we would end up trying to apply
-- another (parsed just before) number. The stack can be empty.
step table sh@(S tt@(t@(Num b):ts) st oo@(os:oss) _) = case sh of
  S _ (Sym _:_)  ((Num _:_):_) _     -> S ts st ((t:os):oss) Inert
  S _ (Node _:_) ((Num _:_):_) _     -> S ts st ((t:os):oss) Inert
  S _ (Op x:_)   ((Num a:_):_) Inert -> case findOps x table of
    [Closed [_] _ SExpression]       -> S ts st ((t:os):oss) Inert
    _                                -> S tt st oo (Done $ CantApply a b)
  S _ _          ((Num a:_):_) Inert -> S tt st oo (Done $ CantApply a b)
  _                                  -> S ts st ((t:os):oss) Inert

-- An applicator is on the input stack.
step table (S (t:ts) st@(s:_) oo@(os:oss) _)
  | applicator table t = case s of
  Op y -> case findOps y table of
    [Closed [_] _ SExpression] -> S ts st ((t:os):oss) SExpr
    _                          -> S ts (t:st) ([]:oo) StackApp
  Sym _                        -> S ts st ((t:os):oss) Inert
  Node _                       -> S ts st ((t:os):oss) Inert
  Num _ -> error "can't happen: Num is handled in a previous equation"

-- An operator part is on the input stack and an applicator is on
-- the stack.
step table (S tt@((Sym x):ts) st@(s:ss) oo _)
  | applicator table s = case findOp x table of
  [Prefix [_] _ _] ->           S ts (Op [x]:st) oo StackL
  [Closed [_] _ SExpression] -> S ts (Op [x]:st) ([]:oo) StackL
  [Closed [_] _ _] ->           S ts (Op [x]:st) oo StackL
  _ -> S tt ss (apply table s oo) FlushApp

-- An operator part is on the input stack and on the stack.
step table (S tt@(t@(Sym x):ts) st@(s@(Op y):ss) oo@(os:oss) ru) =
  let o1 =  head $ findOp x table
      o2 = head $ findOps y table
      pt1 = part o1
      pt2 = part o2
      leftHole1 = leftHole pt1
      rightHole1 = rightHole pt1
      leftHole2 = leftHole pt2
      rightHole2 = rightHole pt2
      (l1,r1) = parts o1
      (l2,r2:_) = parts o2
      new = length l1 == 1
      end = null r1
  in
  case (o1, o2) of
    (Infix [] _ _ _, _) -> error "can't happen"
    (Prefix [] _ _, _) -> error "can't happen"
    (Postfix [] _ _, _) -> error "can't happen"
    (Closed [] _ _, _) -> error "can't happen"

    (Closed [_] _ DistfixAndDiscard, Closed [_] _ SExpression) ->
      S ts            (Op [x]:st)       oo            StackL
    (Closed [_] _ Distfix, Closed [_] _ SExpression) ->
      S ts            (Op [x]:st)       oo            StackL
    (Closed [_] _ SExpression, _) ->
      S ts        (Op [x]:st)         ([]:oo)        StackL
    (Closed _ [] SExpression, Closed _ [_] SExpression)
      | l2++[r2] == l1 && stackedOp ru -> -- TODO build the sym ⟨⟩ (i.e. nil)
        S tt st oo (Done $ EmptyHole (last y) x)
      | l2++[r2] == l1 ->
        S ts                ss                  ((ap:h):oss')           MatchedR
        where (os':h:oss') = oo
              ap = Node (reverse os')
    (_, Closed [_] _ SExpression) ->
      S ts st ((t:os):oss) SExpr

    _ | not new && not (full o2) && not (o1 `continue` o2) ->
       S tt st oo (Done $ Incomplete l2)

    _ | rightHole2 && leftHole1 && stackedOp ru ->
      S tt st oo (Done $ EmptyHole (last y) x)

    _ | o1 `continue` o2 -> S ts (Op l1:ss) oo ContinueOp

    _ | not leftHole1 && new -> S ts (Op [x]:st) oo StackL

    _ -> flushHigher table o1 x tt o2 st oo

-- No more tokens on the input stack, just have to flush
-- the remaining applicators and/or operators.
step table sh@(S [] (s:ss) oo _) = case s of
  Sym _              -> S [] ss (apply table s oo) FlushApp
  Node _             -> S [] ss (apply table s oo) FlushApp
  Op y -> case head $ findOps y table of
    -- The operator has all its parts.
    Infix _ [] _ _ -> S [] ss (apply table s oo) FlushOp
    Prefix _ [] _  -> S [] ss (apply table s oo) FlushOp
    --Postfix _ [] _ -> -- handled by the very first equation
    --Closed _ [] _  -> -- handled by the very first equation
    -- The operator is not complete.
    Infix l r _ _  -> sh { rule = Done $ head r `MissingAfter` l }
    Prefix l r _   -> sh { rule = Done $ head r `MissingAfter` l }
    Postfix l r _  -> sh { rule = Done $ head r `MissingAfter` l }
    Closed l r _   -> sh { rule = Done $ head r `MissingAfter` l }
  Num _ -> error "can't happen: but TODO make a specific data type for the op stack"

-- The applicator/operator stack is empty.
step table sh@(S (t:ts) [] oo ru) = case t of
  Node _ -> S ts [t] ([]:oo) StackApp
  Sym x -> case findOp x table of
    []   -> S ts [t] ([]:oo) StackApp
    -- x is the first sub-op, and the stack is empty
    [Infix [_] _ _ _]
      | ru == Initial -> error $ "missing sub-expression before " ++ x
      | otherwise              -> S ts [Op [x]] oo StackOp
    [Prefix [_] _ _]           -> S ts [Op [x]] oo StackL
    [Postfix [_] [] _] ->
      let ([a]:oss) = oo in       S (Node [Op [x],a]:ts) [] ([]:oss) MakeInert
    [Postfix [_] _ _]          -> S ts [Op [x]] oo StackOp
    [Closed [_] _ SExpression] -> S ts [Op [x]] ([]:oo) StackL
    [Closed [_] _ _] -> S ts [Op [x]] oo StackL
    [Infix l _ _ _]  -> sh { rule = Done $ init l `MissingBefore` last l }
    [Prefix l _ _]   -> sh { rule = Done $ init l `MissingBefore` last l }
    [Postfix l _ _]  -> sh { rule = Done $ init l `MissingBefore` last l }
    [Closed l _ _]   -> sh { rule = Done $ init l `MissingBefore` last l }
    _ -> error "can't happen" -- a single op in the list is returned.
  Num _ -> error "can't happen: Num is handled in a previous equation"
  Op _ -> error "can't happen: but TODO make a specifi data type for the input stack"

-- Everything is done and fine.
step _ sh@(S [] [] [[_]] _) = sh { rule = Done Success }

-- This equation should never be reached; otherwise it is a bug.
step _ sh = sh { rule = Done Unexpected }

applicator :: Table -> Tree -> Bool
applicator table (Sym x) = findOp x table == []
applicator _ (Node _) = True
applicator _ _ = False

flushHigher table o1 x (t:ts) o2 (s:ss) oo
  | o1 `lower` o2 =
    S (t:ts) ss (apply table s oo) FlushOp
  | otherwise =
    S ts (Op [x]:s:ss) oo StackOp

apply :: Table -> Tree -> [[Tree]] -> [[Tree]]
apply table s@(Op y) (os:oss) =
  if length l < nargs
  -- TODO this error case should probably be discovered earlier,
  -- so hitting this point should be a bug.
  then error $ "not enough arguments supplied to " ++ show y
  else (Node (s:reverse l) : r) : oss
  where nargs = case findOps y table of
          [Infix _ _ _ _] -> length y + 1
          [Closed _ _ _] -> length y - 1
          [_] -> length y
          _ -> error $ "bug: wrong use of apply: " ++ show y
        (l,r) = splitAt nargs os
apply _ s@(Sym _) (os:h:oss) =  (ap:h):oss
  where ap = if null os then s else Node (s:reverse os)
apply _ s@(Node _) (os:h:oss) =  (ap:h):oss
  where ap = if null os then s else Node (s:reverse os)
apply _ s oss = error $ "can't apply " ++ show s ++ " to " ++ show oss

