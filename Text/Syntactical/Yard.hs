-- |
-- The modified Shunting-Yard algorithm. The modifications allow function
-- application by juxtaposition (without any paren around the arguments)
-- and distfix operators.

-- TODO make sure the rules reflect what's going on, a same
-- rule should be associated to a same behavior.
-- TODO factorize
-- TODO is ! a + b allowed if ! and + have the same precedence?
-- TODO allow specific operator table for internal operator holes
-- (e.g. to reuse a same symbol with different fixity/precedecence).
-- TODO use HPC to see if tests cover the code.
-- TODO maybe feed random tokens to the algorithm to see if it can crash.
-- TODO use hlint.
-- TODO write more realistic example (for a Haskell-like syntax)

module Text.Syntactical.Yard
  ( shunt, steps, Failure(..), showFailure
  ) where

import Data.List (intersperse)

import Text.Syntactical.Data (
  Tree(..), Op(..), Kind(..), Table,
  begin, end, leftHole, rightHole, rightHoleKind, discard,
  applicator, continue, lower,
  arity, partSymbol, nextPart, previousPart,
  findBoth, findBegin, FindBegin(..)
  )

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
          | Done Result
  deriving (Show, Eq)

data Result =
    Success    -- everything is successfuly parsed
  | Failure Failure
  deriving (Eq, Show)

-- | The different failure cases the 'shunt' function can return.
-- The 'showFailure' function can be used to give them a textual
-- representation.
data Failure =
    MissingBefore [[String]] String -- error case: missing parts before part
  | MissingAfter [String] [String]  -- error case: missing part after parts
  | CantMix Op Op -- error case: can't mix two operators
  | CantApply Int Int -- error case: can't apply number to number
  | EmptyHole String String -- error case: no sub-expression between ops/parts.
  | Incomplete [String] -- error case: missing operator part(s) after the strings.
  | Unexpected -- unexpected state (can't happen, this is a bug)
  deriving (Eq, Show)

isDone :: Shunt -> Bool
isDone (S _ _ _ (Done _)) = True
isDone _ = False

-- | Give a textual representation of a 'Failure'.
showFailure :: Failure -> String
showFailure f = case f of
  MissingAfter p ps -> "Parse error: missing operator part " ++
    concat (intersperse ", " p) ++ " after " ++ concat (intersperse " " ps)
  MissingBefore ps p -> "Parse error: missing operator parts " ++
    concatMap (\pt -> concat (intersperse " " pt)) ps ++ " before " ++ p
  CantMix _ _ -> "Parse error: cannot mix operators"
  CantApply a b -> "Parse error: cannot apply " ++ show a ++ " to " ++ show b
  EmptyHole a b -> "Parse error: no sub-expression between " ++ a ++ " and " ++ b
  Incomplete s -> "Parse error: missing operator parts after " ++ show s
  Unexpected -> "Parsing raised a bug"

failure :: Failure -> Rule
failure f = Done $ Failure f

stackedOp :: Rule -> Bool
stackedOp StackL = True
stackedOp StackOp = True
stackedOp ContinueOp = True
stackedOp _ = False

data Shunt = S
  [Tree]   -- list of tokens (Nodes can be pushed back.)
  [Tree]   -- stack of operators and applicators
  [[Tree]] -- stack of stacks
  Rule

rule :: Shunt -> Rule -> Shunt
rule (S tt st oo _) ru = S tt st oo ru

instance Show Shunt where
  show (S ts ss os ru) =
    pad 20 ts ++ pad 20 ss ++ pad 20 os ++ "   " ++ show ru

pad :: Show a => Int -> a -> [Char]
pad n s = let s' = show s in replicate (n - length s') ' ' ++ s'

-- | Similar to the 'shunt' function but print the steps
-- performed by the modified shunting yard algorithm.
steps :: Table -> [Tree] -> IO ()
steps table ts = do
  putStrLn $ "               Input               Stack              Output   Rule"
  let sh = iterate (step table) $ initial ts
  let l = length $ takeWhile (not . isDone) sh
  mapM_ (putStrLn . show) (take (l + 1) sh)

initial :: [Tree] -> Shunt
initial ts = S ts [] [[]] Initial

-- | Parse a list of tokens according to an operator table.
shunt :: Table -> [Tree] -> Either Failure Tree
shunt table ts = case fix $ initial ts of
  S [] [] [[o']] (Done Success) -> Right o'
  S _ _ _ (Done (Failure f)) -> Left f
  _ -> error "can't happen"
  where fix s = let s' = step table s in
                if isDone s' then s' else fix s'

step :: Table -> Shunt -> Shunt

-- There is a complete Closed or Postifx operator on the top of the stack.
step _ (S tt (s@(Op y):ss) oo@(os:oss) _) | end y && (not $ rightHole y)
  = if discard y
  then let (o:os') = os in S (o:tt) ss (os':oss) MatchedR
  else let ((o:os'):oss') = apply s oo in S (o:tt) ss (os':oss') MatchedR

-- A number is on the input stack. It goes straight
-- to the output unless we would end up trying to apply
-- another (parsed just before) number. The stack can be empty.
step _ sh@(S tt@(t@(Num b):ts) st oo@(os:oss) _) = case sh of
  S _ (Sym _:_)  ((Num _:_):_) _     -> S ts st ((t:os):oss) Inert
  S _ (Node _:_) ((Num _:_):_) _     -> S ts st ((t:os):oss) Inert
  S _ (Op y:_)   ((Num a:_):_) Inert
    | rightHoleKind y == Just SExpression ->
      S ts st ((t:os):oss) Inert
    | otherwise ->
      S tt st oo (failure $ CantApply a b)
  S _ _          ((Num a:_):_) Inert -> S tt st oo (failure $ CantApply a b)
  _                                  -> S ts st ((t:os):oss) Inert

-- An applicator is on the input stack.
step table (S (t:ts) st@(s:_) oo@(os:oss) _)
  | applicator table t = case s of
  Op y
    | rightHoleKind y == Just SExpression ->
      S ts st ((t:os):oss) SExpr
    | otherwise ->
      S ts (t:st) ([]:oo) StackApp
  Sym _                        -> S ts st ((t:os):oss) Inert
  Node _                       -> S ts st ((t:os):oss) Inert
  Num _ -> error "can't happen: Num is handled in a previous equation"

-- An operator part is on the input stack and an applicator is on
-- the stack.
step table (S tt@((Sym x):ts) st@(s:ss) oo _)
  | applicator table s =
  case findBoth table x st of
    Right (Begin pt1)
      | not (leftHole pt1) && rightHoleKind pt1 == Just SExpression ->
      S ts (Op pt1:st) ([]:oo) StackL
      | not (leftHole pt1) ->
      S ts (Op pt1:st) oo StackL
    _ ->
      S tt ss (apply s oo) FlushApp

-- An operator part is on the input stack and on the stack.
step table (S tt@(t@(Sym x):ts) st@(s@(Op y):ss) oo@(os:oss) ru) =
  case findBoth table x st of
    Left pt1 -> go pt1
    Right (Begin pt1) -> go pt1
    Right (MissingBegin ps) ->
      S tt st oo (failure $ Incomplete $ head ps)
    Right NoBegin -> error "can't happen" -- x is in the table for sure
  where
    go pt1
      | rightHoleKind pt1 == Just Distfix && rightHoleKind y == Just SExpression =
      S ts (Op pt1:st) oo StackL
      | rightHoleKind pt1 == Just SExpression =
      S ts (Op pt1:st) ([]:oo) StackL

      | rightHoleKind y == Just SExpression =
      if pt1 `continue` y
      then let (os':h:oss') = oo
               ap = Node (reverse os')
           in if stackedOp ru
              then S (Sym (concat $ previousPart pt1++[x]):ts) ss (h:oss') MakeInert -- build the () symbol
              else S ts (Op pt1:ss) ((ap:h):oss') MatchedR
      else S ts st ((t:os):oss) SExpr

      | rightHole y && leftHole pt1 && stackedOp ru =
      S tt st oo (failure $ EmptyHole (partSymbol y) x)

      | pt1 `continue` y = S ts (Op pt1:ss) oo ContinueOp

      | not (leftHole pt1) && begin pt1 = S ts (Op pt1:st) oo StackL

      | pt1 `lower` y = S tt ss (apply s oo) FlushOp

      | otherwise = S ts (Op pt1:st) oo StackOp

-- No more tokens on the input stack, just have to flush
-- the remaining applicators and/or operators.
step _ sh@(S [] (s:ss) oo _) = case s of
  Sym _              -> S [] ss (apply s oo) FlushApp
  Node _             -> S [] ss (apply s oo) FlushApp
  Op y | end y ->
    -- The infix or prefix operator has all its parts.
    -- The postfix/closed is handled in the first equation.
    S [] ss (apply s oo) FlushOp
       | otherwise ->
    -- The operator is not complete.
    rule sh $ failure $ nextPart y `MissingAfter` [partSymbol y]
  Num _ -> error "can't happen: but TODO make a specific data type for the op stack"

-- The applicator/operator stack is empty.
step table sh@(S (t:ts) [] oo ru) = case t of
  Node _ -> S ts [t] ([]:oo) StackApp
  Sym x -> case findBegin table x of
    NoBegin   -> S ts [t] ([]:oo) StackApp
    -- x is the first sub-op, and the stack is empty
    Begin pt1 -> go x pt1
    MissingBegin xs ->
      rule sh $ failure $ xs `MissingBefore` x
  Num _ -> error "can't happen: Num is handled in a previous equation"
  Op _ -> error "can't happen: but TODO make a specifi data type for the input stack"
  where
    go x pt1
      | leftHole pt1 && ru == Initial =
      error $ "missing sub-expression before " ++ x
      | leftHole pt1 =
      S ts [Op pt1] oo StackOp
      | rightHoleKind pt1 == Just SExpression =
      S ts [Op pt1] ([]:oo) StackL
      | otherwise =
      S ts [Op pt1] oo StackL

-- Everything is done and fine.
step _ sh@(S [] [] [[_]] _) = rule sh $ Done Success

-- This equation should never be reached; otherwise it is a bug.
step _ sh = rule sh $ failure Unexpected

apply :: Tree -> [[Tree]] -> [[Tree]]
apply s@(Op y) (os:oss) =
  if length l < nargs
  -- TODO this error case should probably be discovered earlier,
  -- so hitting this point should be a bug.
  then error $ "not enough arguments supplied to " ++ show y
  else (Node (s:reverse l) : r) : oss
  where nargs = arity y
        (l,r) = splitAt nargs os
apply s@(Sym _) (os:h:oss) =  (ap:h):oss
  where ap = if null os then s else Node (s:reverse os)
apply s@(Node _) (os:h:oss) =  (ap:h):oss
  where ap = if null os then s else Node (s:reverse os)
apply s oss = error $ "can't apply " ++ show s ++ " to " ++ show oss

