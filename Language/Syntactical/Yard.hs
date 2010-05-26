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

module Language.Syntactical.Yard where

import Data.List

import Language.Syntactical.Data

data Rule = Initial
          | Inert -- not an operator or an applicator
          | Application
          | FlushApp
          | StackApp
          | FlushOp
          | StackOp
          | StackL
          | UnmatchedL
          | UnmatchedR
          | MatchedR
          | SExpr
          | Success
          | Blocked
          | Unexpected
  deriving (Show, Eq)

isDone sh = elem (rule sh)
  [Success, Blocked, UnmatchedL, UnmatchedR, Unexpected]

data Shunt = S {
    input :: [Tree]    -- list of token (no Node)
  , stack :: [Tree]    -- stack of operators and applicators
  , output :: [[Tree]] -- stack of stacks
  , rule :: Rule
  }

instance Show Shunt where
  show (S ts ss os r) =
    pad 20 ts ++ pad 20 ss ++ pad 20 os ++ "   " ++ show r

pad n s = let s' = show s in replicate (n - length s') ' ' ++ s'

steps table ts = do
  putStrLn $ "               Input               Stack              Output   Rule"
  let sh = iterate (step table) $ initial ts
  let l = length $ takeWhile (not . isDone) sh
  mapM_ (putStrLn . show) (take (l + 1) sh)

initial ts = S ts [] [[]] Initial

isLeft (Left a) = True
isLeft _ = False
isRight (Right a) = True
isRight _ = False

shunt table ts = case fix $ initial ts of
  S [] [] [[o']] Success -> Right o'
  _ -> Left "TODO error message"
  where fix s = let s' = step table s in
                if isDone s' then s' else fix s'

step :: Table -> Shunt -> Shunt
step table sh = case sh of

  S   ts                (s@(Op y):ss)      ([a]:oss) _ ->
    case findOps y table of
      [Postfix _ [] _] -> S (Node [s,a]:ts)    ss           ([]:oss)       FlushApp
      [Closed _ [] _] -> S (Node [s,a]:ts)    ss           ([]:oss)       FlushApp
      _ -> step' table sh
  _ -> step' table sh

step' table sh = case sh of

  S   (t@(Num _):ts)    ss                  (os:oss)                _ ->
    S ts                ss                  ((t:os):oss)            Inert

  S   (t@(Sym x):ts)    (s@(Sym _):ss)      (os:oss) _ ->
    case findOp x table of
      [] -> S ts        (s:ss)              ((t:os):oss)          Application
      [Prefix [_] _ _] -> S ts   (Op [x]:s:ss) (os:oss)           StackOp
      [Closed [_] _ SExpression] -> S ts   (Op [x]:s:ss) ([]:os:oss)           StackOp
      [Closed [_] _ _] -> S ts   (Op [x]:s:ss) (os:oss)           StackOp
      _ ->  S (t:ts)    ss                  (apply table s $ os:oss)    FlushApp

  S   (t@(Sym x):ts) (s@(Op y):ss) oss      _ ->
    case (findOp x table, findOps y table) of
      ([],[o2@(Closed [_] _ SExpression)]) ->
        S ts                (s:ss)              ((t:os):oss')       SExpr
        where (os:oss') = oss
      ([], _) -> S ts   (t:s:ss)            ([]:oss)             StackApp
      ([Closed [_] _ DistfixAndDiscard],[o2@(Closed [_] _ SExpression)]) ->
        S ts                (Op [x]:s:ss)       (os:oss')           StackApp
        where (os:oss') = oss
      ([Closed [_] _ Distfix],[o2@(Closed [_] _ SExpression)]) ->
        S ts                (Op [x]:s:ss)       (os:oss')           StackApp
        where (os:oss') = oss
      ([o1@(Infix [_] [] _ _)], [o2@(Infix [_] [] _ _)]) ->
        flushLower table o1 x ts (s:ss) oss
      ([o1@(Infix l1 r1 _ _)], [o2@(Infix l2 (r2:r2s) _ _)])
        | l2++[r2] == l1 ->
          S ts      (Op l1:ss)            oss          StackOp
      ([o1@(Infix [_] _ _ _)], [o2@(Infix _ [] _ _)]) ->
        flushLower table o1 x ts (s:ss) oss
      ([o1@(Infix _ _ _ p1)], [o2@(Prefix _ _ p2)]) ->
        flushLower table o1 x ts (s:ss) oss
      ([o1@(Prefix [_] _ _)], [o2@(Infix [_] _ _ _)]) ->
          S ts      (Op [x]:s:ss)         oss          StackOp
      ([o1@(Prefix [_] [] _)], [o2@(Prefix [_] [] _)]) ->
          S ts      (Op [x]:s:ss)         oss          StackOp
      ([o1@(Prefix l1 r1 _)], [o2@(Prefix l2 (r2:r2s) _)])
        | l2++[r2] == l1 ->
          S ts      (Op l1:ss)            oss          StackOp
      ([o1@(Postfix [_] [] p1)], [o2@(Prefix [_] [] p2)])
        | p1 > p2 ->
          S ts      (Op [x]:s:ss)         oss          StackOp
        | p1 < p2 ->
          S ts      (Op [x]:ss)           (apply table s oss) StackOp
        | otherwise -> error $ "precedence cannot be mixed: " ++ show t ++ ", " ++ show s
      ([o1@(Closed [_] _ SExpression)], _) ->
        S ts                (Op [x]:s:ss)              ([]:os:oss')            StackApp
        where (os:oss') = oss

      ([o1@(Closed l1 [] Discard)], [o2@(Closed l2 [r2] Discard)])
        | l2++[r2] == l1 ->
         S (o:ts)           ss             (os:oss')             MatchedR
         where ((o:os):oss') = oss
      ([o1@(Closed l1 [] DistfixAndDiscard)], [o2@(Closed l2 [r2] DistfixAndDiscard)])
        | l2++[r2] == l1 ->
         S (o:ts)           ss             (os:oss')             MatchedR
         where ((o:os):oss') = oss
      ([o1@(Closed l1 [] SExpression)], [o2@(Closed l2 [r2] SExpression)])
        | l2++[r2] == l1 ->
          S ts                ss                  ((ap:h):oss')           MatchedR
          where (os:h:oss') = oss
                ap = Node (reverse os)

      ([o1@(Closed l1 [] _)], [o2@(Closed l2 [r2] _)])
        | l2++[r2] == l1 ->
          S (o:ts)           ss       (os:oss')      MatchedR
          where ((o:os):oss') = apply table (Op l1) oss
      ([o1@(Closed l1 r1 _)], [o2@(Closed l2 (r2:r2s) _)])
        | l2++[r2] == l1 ->
          S ts      (Op l1:ss)            oss          StackOp
      (_,[o2@(Closed [_] _ SExpression)]) ->
        S ts                (s:ss)              ((t:os):oss')           SExpr
        where (os:oss') = oss
      ([o1], [o2@(Closed _ _ _)]) ->
          S ts      (Op [x]:s:ss)         oss          StackOp
      ([o1@(Closed _ [] _)], [_]) ->
          S (t:ts)            ss                  (apply table s oss) FlushOp
      ([o1@(Closed _ _ _)], [_]) ->
          S ts      (Op [x]:s:ss)           oss FlushOp
      _ -> error $ "TODO: " ++ show t ++ ", " ++ show s

  S   (t@(Sym x):ts) (s@(Node _):ss)   (os:oss)              _ ->
    case findOp x table of
      [] -> S ts        (s:ss)              ((t:os):oss)          Application
      _ -> S (t:ts)     ss                  (apply table s $ os:oss)            FlushApp

  S   (t@(Sym x):ts)    ss                  (os:oss)                _ ->
    case findOp x table of
      [] -> S ts       (t:ss)              ([]:os:oss)             StackApp
      -- x is the first sub-op, and the stack is empty or has a left bracket at its top.
      _ -> case findOps [x] table of
        [] -> error $ "using middle sub-op " ++ x ++ " as first sub-op." ++
                      "\nstack: " ++ show ss ++
                      "\noutput: " ++ show (os:oss)
        [Closed [_] _ SExpression] -> S ts   (Op [x]:ss) ([]:os:oss)           StackOp
        _ -> S ts      (Op [x]:ss)  (os:oss)  StackOp

  S   (t@(Node _):ts) (s@(Op y):ss)       (os:oss)               _ ->
    case findOps y table of
      [o2@(Closed [_] _ SExpression)] ->
        S ts              (s:ss)              ((t:os):oss)         SExpr
      _ ->
        S ts              (t:s:ss)            ([]:os:oss)          StackApp

  S   (t@(Node _):ts) (s@(Sym _):ss)        (os:oss)                _ ->
    S ts              (s:ss)                ((t:os):oss)            Application

  S   (t@(Node _):ts) (s@(Node _):ss)       (os:oss)                _ ->
    S ts              (s:ss)                ((t:os):oss)            Application

  S   (t@(Node _):ts)   ss                  (os:oss)                _ ->
    S ts                (t:ss)              ([]:os:oss)             StackApp

  S   []                (s@(Op _):ss)       oss              _ ->
    S []                ss                  (apply table s oss)            FlushOp

  S   []                (s@(Sym _):ss)      oss              _ ->
    S []                ss                  (apply table s oss)            FlushApp

  S   []                (s@(Node _):ss)     oss             _ ->
    S []                ss                  (apply table s oss)            FlushApp

  S   []                []                  [[o]]           _ ->
    S []                []                  [[o]]                    Success

  _ -> sh { rule = Unexpected }

flushLower table o1 x ts (s@(Op y):ss) oss = case findOps y table of
  [o2]
    | o1 `lower` o2 ->
      flushLower table o1 x ts ss (apply table s oss)
    | otherwise ->
      S ts      (Op [x]:s:ss)         oss          StackOp
flushLower table o1 x ts ss oss =
   S ts      (Op [x]:ss)         oss          StackOp

apply table s@(Op y) (os:oss) = (Node (s:reverse l) : r) : oss
  where nargs = case findOps y table of
          [Infix _ _ _ _] -> length y + 1
          [Closed _ _ _] -> length y - 1
          [_] -> length y
          [] -> error $ "bug: wrong use of apply: " ++ show y
        (l,r) = splitAt nargs os -- TODO test correct lenght of os
apply table s@(Sym _) (os:h:oss) =  (ap:h):oss
  where ap = if null os then s else Node (s:reverse os)
apply table s@(Node _) (os:h:oss) =  (ap:h):oss
  where ap = if null os then s else Node (s:reverse os)
apply table s oss = error $ "can't apply " ++ show s ++ " to " ++ show oss

