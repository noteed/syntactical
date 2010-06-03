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

module Language.Syntactical.Yard where

import Data.List (intersperse)

import Language.Syntactical.Data

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
  Unexpected -> "Parsing raised a bug"

stackedOp :: Rule -> Bool
stackedOp StackL = True
stackedOp StackOp = True
stackedOp ContinueOp = True
stackedOp _ = False

data Shunt = S {
    input :: [Tree]    -- list of token (Nodes can be pushed back.)
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
  case (head $ findOp x table, head $ findOps y table) of
    (Infix [] _ _ _, _) -> error "can't happen"
    (Prefix [] _ _, _) -> error "can't happen"
    (Postfix [] _ _, _) -> error "can't happen"
    (Closed [] _ _, _) -> error "can't happen"
    (Closed [_] _ DistfixAndDiscard, Closed [_] _ SExpression) ->
      S ts            (Op [x]:st)       oo            StackL
    (Closed [_] _ Distfix, Closed [_] _ SExpression) ->
      S ts            (Op [x]:st)       oo            StackL
    (Infix l1 _ _ _, Infix l2 (r2:_) _ _)
      | l2++[r2] == l1 && stackedOp ru ->
        S tt st oo (Done $ EmptyHole (last y) x)
      | l2++[r2] == l1 ->
        S ts      (Op l1:ss)            oo          ContinueOp
    (o1@(Infix [_] _ _ _), Infix _ [] _ _)
      | stackedOp ru -> S tt st oo (Done $ EmptyHole (last y) x)
      | otherwise -> flushLower table o1 x ts st oo
    (o1@(Infix _ _ _ _), Prefix _ _ _) ->
      flushLower table o1 x ts st oo
    (Prefix [_] _ _, Infix _ _ _ _) ->
        S ts      (Op [x]:st)         oo          StackL
    (Prefix l1 _ _, Infix _ _ _ _) ->
      S tt st oo (Done $ init l1 `MissingBefore` last l1)
    (Prefix l1 _ _, Prefix l2 (r2:_) _)
      | l2++[r2] == l1 && stackedOp ru ->
        S tt st oo (Done $ EmptyHole (last y) x)
      | l2++[r2] == l1 ->
        S ts      (Op l1:ss)            oo          ContinueOp
      | otherwise ->
        S tt st oo (Done $ missingPrefix l1 l2)
    (Prefix [_] _ _, Prefix _ [] _) ->
        S ts      (Op [x]:st)         oo          StackL
    (o1@(Postfix [_] [] p1), o2@(Prefix [_] [] p2))
      -- TODO use flushLower ?
      | p1 > p2 ->
        let [a] = os
        in S (Node [Op [x],a]:ts) st ([]:oss) MakeInert
      | p1 < p2 ->
        let ([a]:oss') = apply table s oo
        in S (Node [Op [x],a]:ts) ss ([]:oss') MakeInert
      | otherwise -> S tt st oo (Done $ CantMix o1 o2)
    (Closed [_] _ SExpression, _) ->
      S ts        (Op [x]:st)         ([]:oo)        StackL
    (Closed l1 [] Discard, Closed l2 [r2] Discard)
      | l2++[r2] == l1 && stackedOp ru ->
        S tt st oo (Done $ EmptyHole (last y) x)
      | l2++[r2] == l1 ->
       S (o:ts)           ss             (os':oss)             MatchedR
       where (o:os') = os
    (Closed l1 [] DistfixAndDiscard, Closed l2 [r2] DistfixAndDiscard)
      | l2++[r2] == l1 && stackedOp ru ->
        S tt st oo (Done $ EmptyHole (last y) x)
      | l2++[r2] == l1 ->
       S (o:ts)           ss             (os':oss)             MatchedR
       where (o:os') = os
    (Closed l1 [] SExpression, Closed l2 [r2] SExpression)
      | l2++[r2] == l1 && stackedOp ru ->
        S tt st oo (Done $ EmptyHole (last y) x)
      | l2++[r2] == l1 ->
        S ts                ss                  ((ap:h):oss')           MatchedR
        where (os':h:oss') = oo
              ap = Node (reverse os')
    (Closed l1 [] _, Closed l2 (r2:_) _)
      | l2++[r2] == l1 && stackedOp ru ->
        S tt st oo (Done $ EmptyHole (last y) x)
      | l2++[r2] == l1 ->
        S (o:ts)           ss       (os':oss')      MatchedR
      | otherwise ->
        S tt st oo (Done $ missingPrefix l1 l2)
        where ((o:os'):oss') = apply table (Op l1) oo
    (Closed l1 _ _, Closed l2 (r2:_) _)
      | l2++[r2] == l1 && stackedOp ru ->
        S tt st oo (Done $ EmptyHole (last y) x)
      | l2++[r2] == l1 ->
        S ts      (Op l1:ss)            oo          ContinueOp
    (_, Closed [_] _ SExpression) ->
      S ts                st              ((t:os):oss)           SExpr
    (Infix [_] _ _ _, Closed _ _ _)
      | ru == StackL -> error $ "missing sub-expression before " ++ x
    (_, Closed _ _ _) ->
        S ts      (Op [x]:st)         oo          StackOp
    (Closed _ [] _, _) ->
        S tt            ss                  (apply table s oo) FlushOp
    (Closed _ _ _, _) ->
        S ts      (Op [x]:st)           oo StackOp
--    _ -> error $ "TODO: This is a bug: the patterns should be exhaustive but" ++
--           "(" ++ show t ++ ", " ++ show s ++ ") is not matched."

-- No more tokens on the input stack, just have to flush
-- the remaining applicators and/or operators.
step table sh@(S [] (s:ss) oo _) = case s of
  Sym _              -> S [] ss (apply table s oo) FlushApp
  Node _             -> S [] ss (apply table s oo) FlushApp
  Op y -> case head $ findOps y table of
    -- The operator has all its parts.
    Infix _ [] _ _ -> S [] ss (apply table s oo) FlushOp
    Prefix _ [] _  -> S [] ss (apply table s oo) FlushOp
    Postfix _ [] _ -> S [] ss (apply table s oo) FlushOp
    Closed _ [] _  -> S [] ss (apply table s oo) FlushOp
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
    _ -> S ts [Op [x]] oo StackOp
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

flushLower :: Table -> Op -> String -> [Tree] -> [Tree] -> [[Tree]] -> Shunt
flushLower table o1 x ts (s@(Op y):ss) oss = case head $ findOps y table of
  o2
    | o1 `lower` o2 ->
      flushLower table o1 x ts ss (apply table s oss)
    | otherwise ->
      S ts (Op [x]:s:ss) oss FlushOp
flushLower _ _ x ts ss oss =
      S ts (Op [x]:ss)   oss StackOp

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

missingPrefix :: [String] -> [a] -> Done
missingPrefix l1 l2 = init l1' `MissingBefore` last l1'
  where l1' = drop (length l2) l1

