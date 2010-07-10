-- |
-- The modified Shunting-Yard algorithm. The modifications allow function
-- application by juxtaposition (without any paren around the arguments)
-- and distfix operators.

-- TODO make sure the rules reflect what's going on, a same
-- rule should be associated to a same behavior.
-- TODO is ! a + b allowed if ! and + have the same precedence?
-- TODO allow specific operator table for internal operator holes
-- (e.g. to reuse a same symbol with different fixity/precedecence).
-- TODO use HPC to see if tests cover the code.
-- TODO maybe feed random tokens to the algorithm to see if it can crash.
-- TODO use hlint.
-- TODO write more realistic example (for a Haskell-like syntax)

-- Note: The parser allows applying a number to another,
-- e.g. 1 2. Maybe this could be turned into an option.
-- The proper way to forbid such 'number application' is
-- to use some type-checking. If 1 2 should be disallowed,
-- 1 (2 + 3) or 1 a shoule be disallowed too. The 'apply'
-- function seems a good place to implement such restriction.

module Text.Syntactical.Yard
  ( Shunt(..), initial, isDone, shunt, step, Failure(..)
  ) where

import Text.Syntactical.Data (
  SExpr(..), Tree(..), Op(..), Kind(..), Table,
  begin, end, leftHole, rightHole, rightHoleKind, discard,
  applicator, applicator', continue, lower,
  arity, partSymbol, nextPart, previousPart,
  findBoth, findBegin, FindBegin(..),
  Token, operator
  )

-- convert a SExpr to a Tree
s2t :: SExpr a -> Tree a
s2t (Atom x) = Sym x
s2t (List xs) = Node $ map s2t xs

-- convert a Tree to a SExpr (partial function)
t2s :: Tree a -> SExpr a
t2s (Sym x) = Atom x
t2s (Node xs) = List $ map t2s xs
t2s (Part _) = error "can't convert a Tree Part to a SExpr" -- operator is used in this case

-- An applicator is a non-operator symbol that is applied
-- to some arguments. When such a symbol is read, it is
-- placed on the (operator/applicator) stack. If there is
-- already such a symbol on the stack, it goes straight
-- to the output stack (this is the Inert case).
data Rule a = Initial
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
          | Done (Result a)
  deriving (Show, Eq)

data Result a =
    Success    -- everything is successfuly parsed
  | Failure (Failure a)
  deriving (Eq, Show)

-- | The different failure cases the 'shunt' function can return.
-- The 'showFailure' function can be used to give them a textual
-- representation.
data Failure a =
    MissingBefore [[a]] a -- ^ missing parts before part
  | MissingAfter [a] [a]  -- ^ missing parts after parts
  | CantMix (Op a) (Op a) -- ^ can't mix two operators
  | MissingSubBetween a a -- ^ missing sub-expression between parts
  | MissingSubBefore a    -- ^ missing sub-expression before string
  | MissingSubAfter a     -- ^ missing sub-expression after string
  | Unexpected            -- ^ this is a bug if it happens
  deriving (Eq, Show)

isDone :: Shunt a -> Bool
isDone (S _ _ _ (Done _)) = True
isDone _ = False

failure :: Failure a -> Rule a
failure f = Done $ Failure f

stackedOp :: Rule a -> Bool
stackedOp StackL = True
stackedOp StackOp = True
stackedOp ContinueOp = True
stackedOp _ = False

data Shunt a = S
  [SExpr a]   -- list of tokens (Nodes can be pushed back.)
  [Tree a]    -- stack of operators and applicators
  [[SExpr a]] -- stack of stacks
  (Rule a)

rule :: Shunt a -> Rule a -> Shunt a
rule (S tt st oo _) ru = S tt st oo ru

initial :: [SExpr a] -> Shunt a
initial ts = S ts [] [[]] Initial

-- | Parse a list of tokens according to an operator table.
shunt :: Token a => Table a -> [SExpr a] -> Either (Failure a) (SExpr a)
shunt table ts = case fix $ initial ts of
  S [] [] [[o']] (Done Success) -> Right o'
  S _ _ _ (Done (Failure f)) -> Left f
  _ -> error "can't happen"
  where fix s = let s' = step table s in
                if isDone s' then s' else fix s'

step :: Token a => Table a -> Shunt a -> Shunt a

-- There is a complete Closed or Postifx operator on the top of the stack.
step _ (S tt (s@(Part y):ss) oo@(os:oss) _) | end y && (not $ rightHole y)
  = if discard y
  then let (o:os') = os in S (o:tt) ss (os':oss) MatchedR
  else let ((o:os'):oss') = apply s oo in S (o:tt) ss (os':oss') MatchedR

-- An applicator is on the input stack.
step table (S (t:ts) st@(s:_) oo@(os:oss) _)
  | applicator table t = case s of
  Part y
    | rightHoleKind y == Just SExpression ->
      S ts st ((t:os):oss) SExpr
    | otherwise ->
      S ts (s2t t:st) ([]:oo) StackApp
  Sym _                        -> S ts st ((t:os):oss) Inert
  Node _                       -> S ts st ((t:os):oss) Inert

-- An operator part is on the input stack and an applicator is on
-- the stack.
step table (S tt@((Atom x):ts) st@(s:ss) oo _)
  | applicator' table s =
  case findBoth table x st of
    Right (Begin pt1)
      | not (leftHole pt1) && rightHoleKind pt1 == Just SExpression ->
      S ts (Part pt1:st) ([]:oo) StackL
      | not (leftHole pt1) ->
      S ts (Part pt1:st) oo StackL
    _ ->
      S tt ss (apply s oo) FlushApp

-- An operator part is on the input stack and on the stack.
step table sh@(S tt@(t@(Atom x):ts) st@(s@(Part y):ss) oo@(os:oss) ru) =
  case findBoth table x st of
    Left pt1 -> go pt1
    Right (Begin pt1) -> go pt1
    Right (MissingBegin ps) -> rule sh (failure $ ps `MissingBefore` x)
    Right NoBegin -> error "can't happen" -- x is in the table for sure
  where
    go pt1
      | rightHoleKind pt1 == Just Distfix && rightHoleKind y == Just SExpression =
      S ts (Part pt1:st) oo StackL
      | rightHoleKind pt1 == Just SExpression =
      S ts (Part pt1:st) ([]:oo) StackL
      | rightHoleKind y == Just SExpression && pt1 `continue` y && stackedOp ru =
      let ([]:h:oss') = oo
      in S ts (Part pt1:ss) ((List []:h):oss') MatchedR
      | rightHoleKind y == Just SExpression && pt1 `continue` y =
      let (os':h:oss') = oo
          ap = List (reverse os')
      in S ts (Part pt1:ss) ((ap:h):oss') MatchedR
      | rightHoleKind y == Just SExpression =
      S ts st ((t:os):oss) SExpr

      | rightHole y && leftHole pt1 && stackedOp ru =
      rule sh (failure $ partSymbol y `MissingSubBetween` x)

      | pt1 `continue` y = S ts (Part pt1:ss) oo ContinueOp

      | not (leftHole pt1) && begin pt1 = S ts (Part pt1:st) oo StackL

      | pt1 `lower` y = S tt ss (apply s oo) FlushOp

      | otherwise = S ts (Part pt1:st) oo StackOp

-- No more tokens on the input stack, just have to flush
-- the remaining applicators and/or operators.
step _ sh@(S [] (s:ss) oo ru) = case s of
  Sym _              -> S [] ss (apply s oo) FlushApp
  Node _             -> S [] ss (apply s oo) FlushApp
  Part y | end y && rightHole y && stackedOp ru ->
    rule sh (failure $ MissingSubAfter $ partSymbol y)
    -- The infix or prefix operator has all its parts.
    -- The postfix/closed is handled in the first equation.
       | end y ->
    S [] ss (apply s oo) FlushOp
       | otherwise ->
    -- The operator is not complete.
    rule sh (failure $
      nextPart y `MissingAfter` (previousPart y ++ [partSymbol y]))

-- The applicator/operator stack is empty.
step table sh@(S (t:ts) [] oo ru) = case t of
  List _ -> S ts [s2t t] ([]:oo) StackApp
  Atom x -> case findBegin table x of
    NoBegin   -> S ts [s2t t] ([]:oo) StackApp
    -- x is the first sub-op, and the stack is empty
    Begin pt1 -> go pt1
    MissingBegin xs -> rule sh (failure $ xs `MissingBefore` x)
  where
    go pt1
      | leftHole pt1 && ru == Initial =
      rule sh (failure $ MissingSubBefore $ partSymbol pt1)
      | leftHole pt1 =
      S ts [Part pt1] oo StackOp
      | rightHoleKind pt1 == Just SExpression =
      S ts [Part pt1] ([]:oo) StackL
      | otherwise =
      S ts [Part pt1] oo StackL

-- Everything is done and fine.
step _ sh@(S [] [] [[_]] _) = rule sh $ Done Success

-- This equation should never be reached; otherwise it is a bug.
step _ sh = rule sh (failure Unexpected)

apply :: Token a => Tree a -> [[SExpr a]] -> [[SExpr a]]
apply (Part y) (os:oss) =
  if length l < nargs
  -- TODO this error case should probably be discovered earlier,
  -- so hitting this point should be a bug.
  then error $ "not enough arguments supplied to " -- TODO ++ show y
  else (List (operator y:reverse l) : r) : oss
  where nargs = arity y
        (l,r) = splitAt nargs os
apply (Sym x) (os:h:oss) =  (ap:h):oss
  where ap = if null os then Atom x else List (Atom x:reverse os)
apply (Node xs) (os:h:oss) =  (ap:h):oss
  where ap = if null os then List (map t2s xs) else List (List (map t2s xs):reverse os)
apply _ _ = error "can't happen"

