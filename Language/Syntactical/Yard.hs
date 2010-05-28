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
          | StackOp    -- push a new operator part to the stack
          | ContinueOp -- append an operator part to the operator
                       -- at the top of the stack
          | MatchedR   -- handle the last part of a closed operator
          | SExpr      -- build an s-expression
          | Done Done
  deriving (Show, Eq)

data Done =
    Success    -- everything is successfuly parsed
  | UnmatchedL -- TODO have some error states
  | UnmatchedR -- TODO have some error states
  | NotFirst String -- error case: the operator part appears on
                    -- the input but its prefix hasn't been seen
  | MissingBefore [String] String -- error case: missing parts before part
  | MissingAfter String [String]  -- error case: missing part before parts
  | CantMix Op Op -- error case: can't mix two operators
  | Unexpected -- unexpected state (can't happen, this is a bug)
  deriving (Eq, Show)

isDone :: Shunt -> Bool
isDone sh = case rule sh of
  Done _ -> True
  _ -> False

showDone :: Done -> [Char]
showDone d = case d of
  Success -> "Parsing successful"
  UnmatchedL -> "Parse error: missing operator suffix"
  UnmatchedR -> "Parse error: missing operator prefix"
  NotFirst _ -> "Parse error: missing operator prefix"
  MissingBefore ps p -> "Parse error: missing operator parts " ++
    concat (intersperse " " ps) ++ " before " ++ p
  CantMix _ _ -> "Parse error: cannot mix operators"
  Unexpected -> "Parsing raised a bug"

data Shunt = S {
    input :: [Tree]    -- list of token (Nodes can be pushed back.)
  , stack :: [Tree]    -- stack of operators and applicators
  , output :: [[Tree]] -- stack of stacks
  , rule :: Rule
  }

instance Show Shunt where
  show (S ts ss os r) =
    pad 20 ts ++ pad 20 ss ++ pad 20 os ++ "   " ++ show r

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
                if isDone s' then s' else fix s'

step :: Table -> Shunt -> Shunt
step table sh = case sh of

  S   ts                (s@(Op y):ss)      ([a]:oss) _ ->
    case findOps y table of
  -- TODO this should not happen: a MatchedR should occur before:
  -- this state should be produced directly instead of first pushing
  -- the Postfix or the Closed onto the stack then push it back to
  -- the input in this step.
      [Postfix _ [] _] -> S (Node [s,a]:ts) ss ([]:oss) MakeInert
      [Closed _ [] _] ->  S (Node [s,a]:ts) ss ([]:oss) MakeInert
      _ -> step' table sh
  _ -> step' table sh

step' :: Table -> Shunt -> Shunt
step' table sh = case sh of

  S   (t@(Num _):ts)    ss                  (os:oss)                _ ->
    S ts                ss                  ((t:os):oss)            Inert

  S   (t@(Sym x):ts)    (s@(Sym _):ss)      (os:oss) _ ->
    case findOp x table of
      [] -> S ts (s:ss) ((t:os):oss) Inert
      [Prefix [_] _ _] ->           S ts (Op [x]:s:ss) (os:oss)    StackOp
      [Closed [_] _ SExpression] -> S ts (Op [x]:s:ss) ([]:os:oss) StackOp
      [Closed [_] _ _] ->           S ts (Op [x]:s:ss) (os:oss)    StackOp
      _ -> S (t:ts) ss (apply table s $ os:oss) FlushApp

  S   (t@(Sym x):ts) (s@(Op y):ss) oss      _ ->
    case (findOp x table, findOps y table) of
      ([],[Closed [_] _ SExpression]) ->
        S ts            (s:ss)              ((t:os):oss')        SExpr
        where (os:oss') = oss
      ([], _) -> S ts   (t:s:ss)            ([]:oss)             StackApp
      ([Closed [_] _ DistfixAndDiscard],[Closed [_] _ SExpression]) ->
        S ts            (Op [x]:s:ss)       (os:oss')            StackOp
        where (os:oss') = oss
      ([Closed [_] _ Distfix],[Closed [_] _ SExpression]) ->
        S ts            (Op [x]:s:ss)       (os:oss')            StackOp
        where (os:oss') = oss
      ([Infix l1 _ _ _], [Infix l2 (r2:_) _ _])
        | l2++[r2] == l1 ->
          S ts      (Op l1:ss)            oss          ContinueOp
      ([o1@(Infix [_] _ _ _)], [Infix _ [] _ _]) ->
        flushLower table o1 x ts (s:ss) oss
      ([o1@(Infix _ _ _ _)], [Prefix _ _ _]) ->
        flushLower table o1 x ts (s:ss) oss
      ([Prefix [_] _ _], [Infix [_] _ _ _]) ->
          S ts      (Op [x]:s:ss)         oss          StackOp
      ([Prefix l1 _ _], [Prefix l2 (r2:_) _])
        | l2++[r2] == l1 ->
          S ts      (Op l1:ss)            oss          ContinueOp
        | otherwise ->
          S   (t:ts) (s:ss) oss (Done $ missingPrefix l1 l2)
      ([Prefix [_] _ _], [Prefix _ [] _]) ->
          S ts      (Op [x]:s:ss)         oss          StackOp
      ([o1@(Postfix [_] [] p1)], [o2@(Prefix [_] [] p2)])
        -- TODO use flushLower ?
        | p1 > p2 ->
          S ts      (Op [x]:s:ss)         oss          StackOp
        | p1 < p2 ->
          S ts      (Op [x]:ss)           (apply table s oss) StackOp
        | otherwise ->  S (t:ts) (s:ss) oss (Done $ CantMix o1 o2)
      ([Closed [_] _ SExpression], _) ->
        S ts        (Op [x]:s:ss)         ([]:os:oss')        StackOp
        where (os:oss') = oss

      ([Closed l1 [] Discard], [Closed l2 [r2] Discard])
        | l2++[r2] == l1 ->
         S (o:ts)           ss             (os:oss')             MatchedR
         where ((o:os):oss') = oss
      ([Closed l1 [] DistfixAndDiscard], [Closed l2 [r2] DistfixAndDiscard])
        | l2++[r2] == l1 ->
         S (o:ts)           ss             (os:oss')             MatchedR
         where ((o:os):oss') = oss
      ([Closed l1 [] SExpression], [Closed l2 [r2] SExpression])
        | l2++[r2] == l1 ->
          S ts                ss                  ((ap:h):oss')           MatchedR
          where (os:h:oss') = oss
                ap = Node (reverse os)

      ([Closed l1 [] _], [Closed l2 [r2] _])
        | l2++[r2] == l1 ->
          S (o:ts)           ss       (os:oss')      MatchedR
          where ((o:os):oss') = apply table (Op l1) oss
      ([Closed l1 _ _], [Closed l2 (r2:_) _])
        | l2++[r2] == l1 ->
          S ts      (Op l1:ss)            oss          ContinueOp
      (_,[Closed [_] _ SExpression]) ->
        S ts                (s:ss)              ((t:os):oss')           SExpr
        where (os:oss') = oss
      ([_], [Closed _ _ _]) ->
          S ts      (Op [x]:s:ss)         oss          StackOp
      ([Closed _ [] _], [_]) ->
          S (t:ts)            ss                  (apply table s oss) FlushOp
      ([Closed _ _ _], [_]) ->
          S ts      (Op [x]:s:ss)           oss StackOp
      _ -> error $ "TODO: This is a bug: the patterns should be exhaustive but" ++
             "(" ++ show t ++ ", " ++ show s ++ ") is not matched."

  S   (t@(Sym x):ts) (s@(Node _):ss)   (os:oss)              _ ->
    case findOp x table of
      [] -> S ts        (s:ss)              ((t:os):oss)          Application
      _ -> S (t:ts)     ss                  (apply table s $ os:oss)            FlushApp

  S   (t@(Sym x):ts)    ss                  (os:oss)                _ ->
    case findOp x table of
      [] -> S ts       (t:ss)              ([]:os:oss)             StackApp
      -- x is the first sub-op, and the stack is empty or has a left bracket at its top.
      _ -> case findOps [x] table of
        [] -> S (t:ts) ss                  (os:oss)                (Done $ NotFirst x)
        [Closed [_] _ SExpression] -> S ts   (Op [x]:ss) ([]:os:oss)           StackOp
        _ -> S ts      (Op [x]:ss)  (os:oss)  StackOp

  S   (t@(Node _):ts) (s@(Op y):ss)       (os:oss)               _ ->
    case findOps y table of
      [Closed [_] _ SExpression] ->
        S ts              (s:ss)              ((t:os):oss)         SExpr
      _ ->
        S ts              (t:s:ss)            ([]:os:oss)          StackApp

  S   (t@(Node _):ts) (s@(Sym _):ss)        (os:oss)                _ ->
    S ts              (s:ss)                ((t:os):oss)            Application

  S   (t@(Node _):ts) (s@(Node _):ss)       (os:oss)                _ ->
    S ts              (s:ss)                ((t:os):oss)            Application

  S   (t@(Node _):ts)   ss                  (os:oss)                _ ->
    S ts                (t:ss)              ([]:os:oss)             StackApp

  S   []                (s@(Op y):ss)       oss              _ ->
    case findOps y table of
      [Infix _ [] _ _] ->
        S []            ss                  (apply table s oss)            FlushOp
      [Infix l r _ _] ->
        S [] (s:ss) oss (Done $ head r `MissingAfter` l)
      [Prefix _ [] _] ->
        S []            ss                  (apply table s oss)            FlushOp
      [Prefix l r _] ->
        S [] (s:ss) oss (Done $ head r `MissingAfter` l)
      [Postfix _ [] _] ->
        S []            ss                  (apply table s oss)            FlushOp
      [Postfix l r _] ->
        S [] (s:ss) oss (Done $ head r `MissingAfter` l)
      [Closed _ [] _] ->
        S []            ss                  (apply table s oss)            FlushOp
      [Closed l r _] ->
        S [] (s:ss) oss (Done $ head r `MissingAfter` l)

  S   []                (s@(Sym _):ss)      oss              _ ->
    S []                ss                  (apply table s oss)            FlushApp

  S   []                (s@(Node _):ss)     oss             _ ->
    S []                ss                  (apply table s oss)            FlushApp

  S   []                []                  [[o]]           _ ->
    S []                []                  [[o]]                    (Done Success)

  _ -> sh { rule = Done Unexpected }

flushLower :: Table -> Op -> String -> [Tree] -> [Tree] -> [[Tree]] -> Shunt
flushLower table o1 x ts (s@(Op y):ss) oss = case findOps y table of
  [o2]
    | o1 `lower` o2 ->
      flushLower table o1 x ts ss (apply table s oss)
    | otherwise ->
      S ts (Op [x]:s:ss) oss FlushOp
flushLower _ _ x ts ss oss =
      S ts (Op [x]:ss)   oss StackOp

apply :: Table -> Tree -> [[Tree]] -> [[Tree]]
apply table s@(Op y) (os:oss) = (Node (s:reverse l) : r) : oss
  where nargs = case findOps y table of
          [Infix _ _ _ _] -> length y + 1
          [Closed _ _ _] -> length y - 1
          [_] -> length y
          [] -> error $ "bug: wrong use of apply: " ++ show y
        (l,r) = splitAt nargs os -- TODO test correct lenght of os
apply _ s@(Sym _) (os:h:oss) =  (ap:h):oss
  where ap = if null os then s else Node (s:reverse os)
apply _ s@(Node _) (os:h:oss) =  (ap:h):oss
  where ap = if null os then s else Node (s:reverse os)
apply _ s oss = error $ "can't apply " ++ show s ++ " to " ++ show oss

missingPrefix l1 l2 = init l1' `MissingBefore` last l1'
  where l1' = drop (length l2) l1

