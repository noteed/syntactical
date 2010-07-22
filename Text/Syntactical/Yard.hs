-- |
-- The modified Shunting-Yard algorithm. The modifications allow function
-- application by juxtaposition (without any paren around the arguments)
-- and distfix operators.

-- TODO use HPC to see if tests cover the code.
-- TODO use hlint.

-- Note: The parser allows applying a number to another,
-- e.g. 1 2. Maybe this could be turned into an option.
-- The proper way to forbid such 'number application' is
-- to use some type-checking. If 1 2 should be disallowed,
-- 1 (2 + 3) or 1 a shoule be disallowed too. The 'apply'
-- function seems a good place to implement such restriction.

module Text.Syntactical.Yard (
  Shunt(..), Failure(..), Rule(..),
  initial, isDone, shunt, step, steps, showFailure
  ) where

import Data.List (intersperse)

import Text.Syntactical.Data (
  SExpr(..), Tree(..),
  Hole(..), Part(..), Table, Priority(..),
  begin, end, leftOpen, rightOpen, rightHole, discard,
  applicator, applicator', continue, original, priority,
  arity, symbol, next, current,
  findBoth, findBegin, FindBegin(..), FindBoth(..), Ambiguity(..),
  Token, toString, operator,
  showPart, showSExpr, showTree
  )

----------------------------------------------------------------------
-- Data structures to support the shunting-yard algorithm
----------------------------------------------------------------------

-- An applicator is a non-operator (i.e. a symbol or a list) applied
-- to some arguments. When such a symbol is read, it is placed on the
-- operator stack. If there is already such a symbol on the stack, it
-- goes straight to the output stack (this is the Argument case).
data Rule a = Initial
          | Argument    -- straight to the output stack
          | Application -- apply an applicator
          | ApplyOp    -- apply an operator
          | StackApp   -- push an applicator to the stack
          | StackL     -- push the first part of a closed or prefix operator
          | StackOp    -- push a new operator part to the stack
          | ContinueOp -- append an operator part to the operator
                       -- at the top of the stack
          | MatchedR   -- handle the last part of a closed operator
          | SExpr      -- build an s-expression
          | Done (Result a)
  deriving (Show, Eq)

isInitial :: Rule a -> Bool
isInitial Initial = True
isInitial _ = False

stackedOp :: Rule a -> Bool
stackedOp StackL = True
stackedOp StackOp = True
stackedOp ContinueOp = True
stackedOp _ = False

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
  | CantMix (Part a) (Part a) -- ^ can't mix two operators
  | MissingSubBetween a a -- ^ missing sub-expression between parts
  | MissingSubBefore a    -- ^ missing sub-expression before string
  | MissingSubAfter a     -- ^ missing sub-expression after string
  | Ambiguity Ambiguity   -- ^ the part can be the last or not
  | Unexpected            -- ^ this is a bug if it happens
  deriving (Eq, Show)

failure :: Failure a -> Rule a
failure f = Done $ Failure f

-- The state of the shunting-yard. The input and output types are the same.
-- The operator stack can hold parts in addition to the atoms and lists.
-- This imply conversions (using s2t and t2s) that would be avoided by
-- using the Tree type for the input and the output. But ruling out the
-- invalid input and output (those containing parts) seems better.
data Shunt a = S
  [SExpr a]   -- list of tokens (Nodes can be pushed back.)
  [Tree a]    -- stack of operators and applicators
  [[SExpr a]] -- stack of stacks
  (Rule a)

isDone :: Shunt a -> Bool
isDone (S _ _ _ (Done _)) = True
isDone _ = False

-- Set the rule of a Shunt structure.
rule :: Shunt a -> Rule a -> Shunt a
rule (S tt st oo _) ru = S tt st oo ru

-- Construct the initial state of the shunting-yard from a given input list.
initial :: [SExpr a] -> Shunt a
initial ts = S ts [] [[]] Initial

----------------------------------------------------------------------
-- The modified shunting-yard algorithm
----------------------------------------------------------------------

-- | Parse a list of tokens according to an operator table.
shunt :: Token a => Table a -> [SExpr a] -> Either (Failure a) (SExpr a)
shunt table ts = case fix $ initial ts of
  S [] [] [[o']] (Done Success) -> Right o'
  S _ _ _ (Done (Failure f)) -> Left f
  _ -> error "can't happen" -- the Success case has only the previous form.
  where fix s = let s' = step table s in
                if isDone s' then s' else fix s'

-- Perfom one step of the shunting-yard, moving it from one state to the next.
step :: Token a => Table a -> Shunt a -> Shunt a

-- There is a complete Closed or Postifx operator on the top of the stack.
step _ (S tt (s@(Part y):ss) oo@(os:oss) _) | end y && (not $ rightOpen y)
  = if discard y
  then let (o:os') = os in S (o:tt) ss (os':oss) MatchedR
  else let ((o:os'):oss') = apply s oo in S (o:tt) ss (os':oss') MatchedR

-- An applicator is on the input stack.
step table (S (t:ts) st@(s:_) oo@(os:oss) _)
  | applicator table t = case s of
  Part y
    | rightHole y == Just SExpression ->
      S ts st ((t:os):oss) SExpr
    | otherwise ->
      S ts (s2t t:st) ([]:oo) StackApp
  Leaf _                       -> S ts st ((t:os):oss) Argument
  Branch _                     -> S ts st ((t:os):oss) Argument

-- An operator part is on the input stack and an applicator is on
-- the stack.
step table (S tt@((Atom x):ts) st@(s:ss) oo _)
  | applicator' table s =
  case findBoth table x st of
    BBegin pt1
      | not (leftOpen pt1) && rightHole pt1 == Just SExpression ->
      S ts (Part pt1:st) ([]:oo) StackL
      | not (leftOpen pt1) ->
      S ts (Part pt1:st) oo StackL
    _ ->
      S tt ss (apply s oo) Application

-- An operator part is on the input stack and on the stack.
step table sh@(S tt@(t@(Atom x):ts) st@(s@(Part y):ss) oo@(os:oss) ru) =
  case findBoth table x st of
    BContinue pt1 -> go pt1
    BBegin pt1 -> go pt1
    BMissingBegin ps -> rule sh (failure $ ps `MissingBefore` x)
    BNothing -> error "can't happen" -- x is in the table for sure
    BAmbiguous amb -> rule sh (failure $ Ambiguity amb)
  where
    go pt1
      | rightHole y == Just SExpression && pt1 `continue` y && stackedOp ru =
      let ([]:h:oss') = oo
      in S ts (Part pt1:ss) ((List []:h):oss') ContinueOp
      | rightHole y == Just SExpression && pt1 `continue` y =
      let (os':h:oss') = oo
          ap = List (reverse os')
      in S ts (Part pt1:ss) ((ap:h):oss') ContinueOp
      | rightHole pt1 == Just Distfix && rightHole y == Just SExpression =
      S ts (Part pt1:st) oo StackL
      | rightHole pt1 == Just SExpression =
      S ts (Part pt1:st) ([]:oo) StackL
      | rightHole y == Just SExpression =
      S ts st ((t:os):oss) SExpr

      | rightOpen y && leftOpen pt1 && stackedOp ru =
      rule sh (failure $ symbol y `MissingSubBetween` x)

      | pt1 `continue` y = S ts (Part pt1:ss) oo ContinueOp

      | not (leftOpen pt1) && begin pt1 = S ts (Part pt1:st) oo StackL

      | otherwise = case pt1 `priority` y of
        Lower -> S tt ss (apply s oo) ApplyOp
        Higher -> S ts (Part pt1:st) oo StackOp
        NoPriority -> rule sh (failure $ CantMix pt1 y)

-- No more tokens on the input stack, just have to flush
-- the remaining applicators and/or operators.
step _ sh@(S [] (s:ss) oo ru) = case s of
  Leaf _             -> S [] ss (apply s oo) Application
  Branch _           -> S [] ss (apply s oo) Application
  Part y | end y && rightOpen y && stackedOp ru ->
    rule sh (failure $ MissingSubAfter $ symbol y)
    -- The infix or prefix operator has all its parts.
    -- The postfix/closed is handled in the first equation.
       | end y ->
    S [] ss (apply s oo) ApplyOp
       | otherwise ->
    -- The operator is not complete.
    rule sh (failure $
      next y `MissingAfter` current y)

-- The applicator/operator stack is empty.
step table sh@(S (t:ts) [] oo ru) = case t of
  List _ -> S ts [s2t t] ([]:oo) StackApp
  Atom x -> case findBegin table x of
    NoBegin   -> S ts [s2t t] ([]:oo) StackApp
    -- x is the first sub-op, and the stack is empty
    Begin pt1 -> go pt1
    MissingBegin xs -> rule sh (failure $ xs `MissingBefore` x)
    AmbiguousBegin amb -> rule sh (failure $ Ambiguity amb)
  where
    go pt1
      | leftOpen pt1 && isInitial ru =
      rule sh (failure $ MissingSubBefore $ symbol pt1)
      | leftOpen pt1 =
      S ts [Part pt1] oo StackOp
      | rightHole pt1 == Just SExpression =
      S ts [Part pt1] ([]:oo) StackL
      | otherwise =
      S ts [Part pt1] oo StackL

-- Everything is done and fine.
step _ sh@(S [] [] [[_]] _) = rule sh $ Done Success

-- This equation should never be reached; otherwise it is a bug.
step _ sh = rule sh (failure Unexpected)

-- Construct a new output stack by applying an operator,
-- a symbol, or a list to the top of the output stack.
apply :: Token a => Tree a -> [[SExpr a]] -> [[SExpr a]]
apply (Part y) (os:oss) | end y =
  if length l /= nargs
  then error $ "can't happen" -- holes are always filled by one expression
  else (operator (original y) (reverse l) : r) : oss
  where nargs = arity y
        (l,r) = splitAt nargs os
apply (Leaf x) (os:h:oss) =  (ap:h):oss
  where ap = if null os then Atom x else List (Atom x:reverse os)
apply (Branch xs) (os:h:oss) =  (ap:h):oss
  where ap = if null os then List (map t2s xs) else List (List (map t2s xs):reverse os)
apply _ _ = error "can't happen"

----------------------------------------------------------------------
-- Visualize the sunting-yard algorithm steps
----------------------------------------------------------------------

-- | Similar to the 'shunt' function but print the steps
-- performed by the modified shunting yard algorithm.
steps :: Token a => Table a -> [SExpr a] -> IO ()
steps table ts = do
  putStrLn $ "               Input               Stack              Output   Rule"
  let sh = iterate (step table) $ initial ts
      l = length $ takeWhile (not . isDone) sh
  mapM_ (putStrLn . showShunt) (take (l + 1) sh)

----------------------------------------------------------------------
-- Convenience functions used in step and apply
----------------------------------------------------------------------

-- Convert a SExpr to a Tree
s2t :: SExpr a -> Tree a
s2t (Atom x) = Leaf x
s2t (List xs) = Branch $ map s2t xs

-- Convert a Tree to a SExpr (partial function)
t2s :: Tree a -> SExpr a
t2s (Leaf x) = Atom x
t2s (Branch xs) = List $ map t2s xs
-- The 'operator' function is used in this case
t2s (Part _) = error "can't convert a Tree Part to a SExpr"

----------------------------------------------------------------------
-- A few 'show' functions for Failure, Rule, and Shunt
----------------------------------------------------------------------

-- | Give a textual representation of a 'Failure'.
showFailure :: Token a => Failure a -> String
showFailure f = case f of
  MissingBefore ps p ->
    "Parse error: missing operator parts " ++
    concatMap (\pt -> concat (intersperse " " $ map toString pt)) ps ++
    " before " ++ toString p
  MissingAfter p ps ->
    "Parse error: missing operator part " ++
    concat (intersperse ", " $ map toString p) ++ " after " ++
    concat (intersperse " " $ map toString ps)
  CantMix a b ->
     "Parse error: cannot mix operators " ++ showPart a ++
     " and " ++ showPart b
  MissingSubBetween a b ->
    "Parse error: no sub-expression between " ++ toString a ++
    " and " ++ toString b
  MissingSubBefore a ->
    "Parse error: no sub-expression before " ++ toString a
  MissingSubAfter a ->
    "Parse error: no sub-expression after " ++ toString a
  Ambiguity _ ->
    "Parse error: the symbol is an ambiguous part"
  Unexpected ->
    "Parsing raised a bug"

showRule :: Token a => Rule a -> String
showRule ru = case ru of
  Initial     -> "Initial"
  Argument    -> "Argument"
  Application -> "Application"
  StackApp    -> "StackApp"
  ApplyOp     -> "ApplyOp"
  StackL      -> "StackL"
  StackOp     -> "StackOp"
  ContinueOp  -> "ContinueOp"
  MatchedR    -> "MatchedR"
  SExpr       -> "SExpr"
  Done result -> case result of
    Success   -> "Success"
    Failure f -> "Failure:\n" ++ showFailure f

showShunt :: Token a => Shunt a -> String
showShunt (S ts ss os ru) =
  pad 20 ts ++ pad' 20 ss ++ pads 20 os ++ "   " ++ showRule ru

bracket :: [String] -> String
bracket s = "[" ++ (concat . intersperse ",") s ++ "]"

pad' :: Token a => Int -> [Tree a] -> String
pad' n s =
  let s' = bracket . map showTree $ s
  in replicate (n - length s') ' ' ++ s'

pad :: Token a => Int -> [SExpr a] -> String
pad n s =
  let s' = bracket . map showSExpr $ s
  in replicate (n - length s') ' ' ++ s'

pads :: Token a => Int -> [[SExpr a]] -> String
pads n s =
  let s' = bracket .
        map (bracket . map showSExpr) $ s
  in replicate (n - length s') ' ' ++ s'


