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

data Tree = Node [Tree]
-- The to-be-shunted tokens. Only the information for the
-- shunting yard algorithm is represented. Actual tokens should
-- be converted to this representation.
           | Num Int
           | Sym String
           | Op [String] -- on the stack, TODO turn into Sym on the output

data Op = Infix [String] [String] Associativity Precedence -- infix
        | Prefix [String] [String] Precedence -- prefix
        | Postfix [String] [String] Precedence -- postfix
        | Closed [String] [String] Kind
        -- TODO SExpression so the user can choose the brackets for s-expr
  deriving Show

data Kind = Discard | Keep | SExpression | Distfix | DistfixAndDiscard
  deriving Show

data Associativity = NonAssociative | LeftAssociative | RightAssociative
  deriving (Show, Eq)

type Precedence = Int

instance Show Tree where
  show = display

display :: Tree -> String
display = tail . display'
  where
  display' (Num i) = ' ' : show i
  display' (Sym s) = ' ' : s
  display' (Op l) = ' ' : concat l
  display' (Node es) = ' ' : '⟨' : tail (concatMap display' es) ++ "⟩"

associativity (Infix _ _ a _) = a

prec (Infix _ _ _ p) = p

nonAssoc = (NonAssociative ==) . associativity
lAssoc = (LeftAssociative ==) . associativity
rAssoc = (RightAssociative ==) . associativity

isInfix (Infix _ _ _ _) = True
isInfix _ = False

isInfix' (Infix xs _ _ _) ys = xs == ys
isInfix' _ _ = False

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

steps s = do
  putStrLn $ "               Input               Stack              Output   Rule"
  let sh = iterate shunt $ initial s
  let l = length $ takeWhile (not . isDone) sh
  mapM_ (putStrLn . show) (take (l + 1) sh)

initial s = S (map token $ tokenize s) [] [[]] Initial

parse ts = fix $ initial ts
  where fix s = let s' = shunt s in
                if isDone s' then s' else fix s'

isLeft (Left a) = True
isLeft _ = False
isRight (Right a) = True
isRight _ = False

shunt :: Shunt -> Shunt
shunt sh = case sh of

  S   ts                (s@(Op y):ss)      ([a]:oss) _ ->
    case findOps y someTable of
      [Postfix _ [] _] -> S (Node [s,a]:ts)    ss           ([]:oss)       FlushApp
      [Closed _ [] _] -> S (Node [s,a]:ts)    ss           ([]:oss)       FlushApp
      _ -> shunt' sh
  _ -> shunt' sh

shunt' sh = case sh of

  S   (t@(Num _):ts)    ss                  (os:oss)                _ ->
    S ts                ss                  ((t:os):oss)            Inert

  S   (t@(Sym x):ts)    (s@(Sym _):ss)      (os:oss) _ ->
    case findOp x someTable of
      [] -> S ts        (s:ss)              ((t:os):oss)          Application
      [Prefix [_] _ _] -> S ts   (Op [x]:s:ss) (os:oss)           StackOp
      [Closed [_] _ SExpression] -> S ts   (Op [x]:s:ss) ([]:os:oss)           StackOp
      [Closed [_] _ _] -> S ts   (Op [x]:s:ss) (os:oss)           StackOp
      _ ->  S (t:ts)    ss                  (apply s $ os:oss)    FlushApp

  S   (t@(Sym x):ts) (s@(Op y):ss) oss      _ ->
    case (findOp x someTable, findOps y someTable) of
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
        flushLower o1 x ts (s:ss) oss
      ([o1@(Infix l1 r1 _ _)], [o2@(Infix l2 (r2:r2s) _ _)])
        | l2++[r2] == l1 ->
          S ts      (Op l1:ss)            oss          StackOp
      ([o1@(Infix [_] _ _ _)], [o2@(Infix _ [] _ _)])
        | o1 `lower` o2 ->
          -- TODO possibly flush more ops
          S ts      (Op [x]:ss)           (apply s oss) StackOp
        | otherwise ->
          S ts      (Op [x]:s:ss)         oss          StackOp
      ([o1@(Infix _ _ _ p1)], [o2@(Prefix _ _ p2)])
        | p1 > p2 ->
          S ts      (Op [x]:s:ss)         oss          StackOp
        | p1 < p2 ->
          S ts      (Op [x]:ss)           (apply s oss) StackOp
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
          S ts      (Op [x]:ss)           (apply s oss) StackOp
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
          where ((o:os):oss') = apply (Op l1) oss
      ([o1@(Closed l1 r1 _)], [o2@(Closed l2 (r2:r2s) _)])
        | l2++[r2] == l1 ->
          S ts      (Op l1:ss)            oss          StackOp
      (_,[o2@(Closed [_] _ SExpression)]) ->
        S ts                (s:ss)              ((t:os):oss')           SExpr
        where (os:oss') = oss
      ([o1], [o2@(Closed _ _ _)]) ->
          S ts      (Op [x]:s:ss)         oss          StackOp
      ([o1@(Closed _ [] _)], [_]) ->
          S (t:ts)            ss                  (apply s oss) FlushOp
      ([o1@(Closed _ _ _)], [_]) ->
          S ts      (Op [x]:s:ss)           oss FlushOp
      _ -> error $ "TODO: " ++ show t ++ ", " ++ show s

  S   (t@(Sym x):ts) (s@(Node _):ss)   (os:oss)              _ ->
    case findOp x someTable of
      [] -> S ts        (s:ss)              ((t:os):oss)          Application
      _ -> S (t:ts)     ss                  (apply s $ os:oss)            FlushApp

  S   (t@(Sym x):ts)    ss                  (os:oss)                _ ->
    case findOp x someTable of
      [] -> S ts       (t:ss)              ([]:os:oss)             StackApp
      -- x is the first sub-op, and the stack is empty or has a left bracket at its top.
      _ -> case findOps [x] someTable of
        [] -> error $ "using middle sub-op " ++ x ++ " as first sub-op." ++
                      "\nstack: " ++ show ss ++
                      "\noutput: " ++ show (os:oss)
        [Closed [_] _ SExpression] -> S ts   (Op [x]:ss) ([]:os:oss)           StackOp
        _ -> S ts      (Op [x]:ss)  (os:oss)  StackOp

  S   (t@(Node _):ts) (s@(Op y):ss)       (os:oss)               _ ->
    case findOps y someTable of
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
    S []                ss                  (apply s oss)            FlushOp

  S   []                (s@(Sym _):ss)      oss              _ ->
    S []                ss                  (apply s oss)            FlushApp

  S   []                (s@(Node _):ss)     oss             _ ->
    S []                ss                  (apply s oss)            FlushApp

  S   []                []                  [[o]]           _ ->
    S []                []                  [[o]]                    Success

  _ -> sh { rule = Unexpected }

lower o1@(Infix [_] _ _ _) o2@(Infix _ [] _ _)
    | nonAssoc o1 || (lAssoc o1 && prec o1 <= prec o2) = True
    | rAssoc o1 && prec o1 < prec o2 = True
lower _ _ = False

flushLower o1 x ts (s@(Op y):ss) oss = case findOps y someTable of
  [o2@(Infix [_] [] _ _)]
    | o1 `lower` o2 ->
      flushLower o1 x ts ss (apply s oss)
    | otherwise ->
      S ts      (Op [x]:s:ss)         oss          StackOp
flushLower o1 x ts ss oss =
   S ts      (Op [x]:ss)         oss          StackOp

tokenize = words . tokenize'
tokenize' ('(':cs) = " ( " ++ tokenize' cs
tokenize' (')':cs) = " ) " ++ tokenize' cs
tokenize' ('⟨':cs) = " ⟨ " ++ tokenize' cs
tokenize' ('⟩':cs) = " ⟩ " ++ tokenize' cs
tokenize' (c:cs) = c : tokenize' cs
tokenize' [] = []

token (c:cs) | c `elem` ['a'..'z'] ++ "()⟨⟩+-*/?:#i°%!<>[]|," = Sym (c:cs)
             | otherwise = Num (read [c])

someTable =
 [ Closed [] ["(",")"] DistfixAndDiscard
 , Closed [] ["⟨","⟩"] SExpression
 , Infix [] ["<<"] LeftAssociative 5
 , Infix [] [">>"] LeftAssociative 5
 , Infix [] ["+"] LeftAssociative 6
 , Infix [] ["-"] LeftAssociative 6
 , Infix [] ["*"] LeftAssociative 7
 , Infix [] ["/"] LeftAssociative 7
 , Infix [] ["?",":"] RightAssociative 5
 , Infix [] ["?'", ":'"] RightAssociative 9
 , Prefix [] ["#"] 8
 , Postfix [] ["°"] 7
 , Postfix [] ["%"] 8
 , Postfix [] ["!"] 9
 , Prefix [] ["if","then","else"] 1
 , Closed [] ["</","/>"] Keep
 , Closed [] ["[","|","]"] Keep
 , Infix [] [","] RightAssociative 1
 ]

findOp op [] = []
findOp op (Infix [] parts a p:xs)
  | op `elem` parts =
     let (l,r) = break' (== op) parts
     in Infix l r a p : findOp op xs
  | otherwise = findOp op xs
findOp op (Prefix [] parts p:xs)
  | op `elem` parts =
     let (l,r) = break' (== op) parts
     in Prefix l r p : findOp op xs
  | otherwise = findOp op xs
findOp op (Postfix [] parts p:xs)
  | op `elem` parts =
     let (l,r) = break' (== op) parts
     in Postfix l r p : findOp op xs
  | otherwise = findOp op xs
findOp op (Closed [] parts k:xs)
  | op `elem` parts =
     let (l,r) = break' (== op) parts
     in Closed l r k : findOp op xs
  | otherwise = findOp op xs

findOps ops [] = []
findOps ops (Infix [] parts a p:xs)
  | ops `isPrefixOf` parts = Infix ops (drop (length ops) parts) a p : findOps ops xs
  | otherwise = findOps ops xs
findOps ops (Prefix [] parts p:xs)
  | ops `isPrefixOf` parts = Prefix ops (drop (length ops) parts) p : findOps ops xs
  | otherwise = findOps ops xs
findOps ops (Postfix [] parts p:xs)
  | ops `isPrefixOf` parts = Postfix ops (drop (length ops) parts) p : findOps ops xs
  | otherwise = findOps ops xs
findOps ops (Closed [] parts k:xs)
  | ops `isPrefixOf` parts = Closed ops (drop (length ops) parts) k : findOps ops xs
  | otherwise = findOps ops xs

break' p ls = case break p ls of
  (_, []) -> error "break': no element in l satisfying p"
  (l, r) -> (l ++ [head r], tail r)

apply s@(Op y) (os:oss) = (Node (s:reverse l) : r) : oss
  where nargs = case findOps y someTable of
          [Infix _ _ _ _] -> length y + 1
          [Closed _ _ _] -> length y - 1
          [_] -> length y
          [] -> error $ "bug: wrong use of apply: " ++ show y
        (l,r) = splitAt nargs os -- TODO test correct lenght of os
apply s@(Sym _) (os:h:oss) =  (ap:h):oss
  where ap = if null os then s else Node (s:reverse os)
apply s@(Node _) (os:h:oss) =  (ap:h):oss
  where ap = if null os then s else Node (s:reverse os)
apply s oss = error $ "can't apply " ++ show s ++ " to " ++ show oss

-- [(input, expected output)]
tests :: [(String,String)]
tests = [
  ("1","1"),
  ("a","a"),

  ("1 + 2","⟨+ 1 2⟩"),
  ("a + 2","⟨+ a 2⟩"),
  ("1 + b","⟨+ 1 b⟩"),
  ("a + b","⟨+ a b⟩"),
  ("1 * 2","⟨* 1 2⟩"),

  ("1 + 2 + 3","⟨+ ⟨+ 1 2⟩ 3⟩"),
  ("1 + 2 * 3","⟨+ 1 ⟨* 2 3⟩⟩"),
  ("1 * 2 + 3","⟨+ ⟨* 1 2⟩ 3⟩")

  , ("0 << 1 + 2 * 3", "⟨<< 0 ⟨+ 1 ⟨* 2 3⟩⟩⟩")
  , ("0 + 1 << 2 * 3", "⟨<< ⟨+ 0 1⟩ ⟨* 2 3⟩⟩")
  , ("0 + 1 * 2 << 3", "⟨<< ⟨+ 0 ⟨* 1 2⟩⟩ 3⟩")
  , ("0 << 1 * 2 + 3", "⟨<< 0 ⟨+ ⟨* 1 2⟩ 3⟩⟩")
  , ("0 * 1 << 2 + 3", "⟨<< ⟨* 0 1⟩ ⟨+ 2 3⟩⟩")
  , ("0 * 1 + 2 << 3", "⟨<< ⟨+ ⟨* 0 1⟩ 2⟩ 3⟩")
  , ("(0 << 1) + 2 * 3", "⟨+ ⟨<< 0 1⟩ ⟨* 2 3⟩⟩")
  , ("0 << (1 + 2) * 3", "⟨<< 0 ⟨* ⟨+ 1 2⟩ 3⟩⟩")
  , ("0 << 1 + (2 * 3)", "⟨<< 0 ⟨+ 1 ⟨* 2 3⟩⟩⟩")
  , ("((0 << 1) + 2) * 3", "⟨* ⟨+ ⟨<< 0 1⟩ 2⟩ 3⟩")
  , ("(((0 << 1) + 2) * 3)", "⟨* ⟨+ ⟨<< 0 1⟩ 2⟩ 3⟩")
  , ("⟨<< 0 1⟩ + 2 * 3", "⟨+ ⟨<< 0 1⟩ ⟨* 2 3⟩⟩")
  , ("⟨+ ⟨<< 0 1⟩ 2⟩ * 3", "⟨* ⟨+ ⟨<< 0 1⟩ 2⟩ 3⟩")

  , ("f a","⟨f a⟩"),
  ("f 1","⟨f 1⟩"),
  ("f a b","⟨f a b⟩"),
  ("f 1 b","⟨f 1 b⟩"),
  ("f a 1","⟨f a 1⟩"),

  ("f a + 1","⟨+ ⟨f a⟩ 1⟩"),
  ("1 + f a","⟨+ 1 ⟨f a⟩⟩"),

  ("(a)","a"),
  ("((a))","a"),
  ("1 + (a)","⟨+ 1 a⟩"),
  ("1 + ((a))","⟨+ 1 a⟩"),
  ("(1 + 2)","⟨+ 1 2⟩"),
  ("(1 + (a))","⟨+ 1 a⟩"),
  ("(1 + ((a)))","⟨+ 1 a⟩"),

  ("1 * (2 + 3)","⟨* 1 ⟨+ 2 3⟩⟩"),
  ("(1 + 2) * 3","⟨* ⟨+ 1 2⟩ 3⟩"),
  ("1 + (f a)","⟨+ 1 ⟨f a⟩⟩"),
  ("(f a) + 1","⟨+ ⟨f a⟩ 1⟩"),
  ("(f a b) 1","⟨⟨f a b⟩ 1⟩"),
  ("(f a b) 1 2","⟨⟨f a b⟩ 1 2⟩"),
  ("1 + (f a) 2","⟨+ 1 ⟨⟨f a⟩ 2⟩⟩")
  , ("f (a + b) (1 - 2)", "⟨f ⟨+ a b⟩ ⟨- 1 2⟩⟩")

  , ("⟨1⟩", "⟨1⟩")
  , ("⟨a⟩", "⟨a⟩")
  , ("⟨⟨1⟩⟩", "⟨⟨1⟩⟩")
  , ("⟨⟨a⟩⟩", "⟨⟨a⟩⟩")
  , ("⟨+ a b⟩", "⟨+ a b⟩")
  , ("⟨+ a b⟩ * (1 - 2)", "⟨* ⟨+ a b⟩ ⟨- 1 2⟩⟩")
  , ("(a + b) * ⟨- 1 2⟩", "⟨* ⟨+ a b⟩ ⟨- 1 2⟩⟩")
  , ("⟨* (a + b) (1 - 2)⟩", "⟨* ⟨+ a b⟩ ⟨- 1 2⟩⟩")
  , ("⟨* (a + b) ⟨- 1 2⟩⟩", "⟨* ⟨+ a b⟩ ⟨- 1 2⟩⟩")

  , ("true ? 1 : 0", "⟨?: true 1 0⟩") -- TODO this sould be _?_:_ or __?__:__ or ␣?␣:␣
  , ("true ? 1 : 0 + 1", "⟨?: true 1 ⟨+ 0 1⟩⟩")
  , ("true ?' 1 :' 0 + 1", "⟨+ ⟨?':' true 1 0⟩ 1⟩")

  , ("# a", "⟨# a⟩")
  , ("a # b", "⟨a ⟨# b⟩⟩")
  , ("# # a", "⟨# ⟨# a⟩⟩")

  , ("a !", "⟨! a⟩")
  , ("a ! b", "⟨⟨! a⟩ b⟩")
  , ("a ! !", "⟨! ⟨! a⟩⟩")

  , ("# a °", "⟨° ⟨# a⟩⟩")
--  , ("# a %", Error "precedence cannot be mixed")
  , ("# a !", "⟨# ⟨! a⟩⟩")

  , ("if true then 1 else 0", "⟨ifthenelse true 1 0⟩")
  , ("if 2 then 1 else 0", "⟨ifthenelse 2 1 0⟩")
  , ("if a b then 1 else 0", "⟨ifthenelse ⟨a b⟩ 1 0⟩")
  , ("if true then a b else 0", "⟨ifthenelse true ⟨a b⟩ 0⟩")
  , ("if true then 1 else a b", "⟨ifthenelse true 1 ⟨a b⟩⟩")
  , ("1 + if true then 1 else 0", "⟨+ 1 ⟨ifthenelse true 1 0⟩⟩")
  , ("1 + if true then 1 else a b + c", "⟨+ 1 ⟨ifthenelse true 1 ⟨+ ⟨a b⟩ c⟩⟩⟩")
  , ("f if true then 1 else 0", "⟨f ⟨ifthenelse true 1 0⟩⟩")

  , ("</ a />","⟨<//> a⟩")
  , ("</ 0 />","⟨<//> 0⟩")
  , ("</ f a b />","⟨<//> ⟨f a b⟩⟩")
  , ("</ f 1 2 />","⟨<//> ⟨f 1 2⟩⟩")
  , ("</ a + b />","⟨<//> ⟨+ a b⟩⟩")
  , ("</ a + b * 2 />","⟨<//> ⟨+ a ⟨* b 2⟩⟩⟩")
  , ("</ a /> + 1","⟨+ ⟨<//> a⟩ 1⟩")
  , ("1 + </ a />","⟨+ 1 ⟨<//> a⟩⟩")
  , ("1 + </ a - b /> * 2","⟨+ 1 ⟨* ⟨<//> ⟨- a b⟩⟩ 2⟩⟩")
  , ("</ a + b /> c","⟨⟨<//> ⟨+ a b⟩⟩ c⟩")
  , ("f </ a />","⟨f ⟨<//> a⟩⟩")
  , ("f </ a + b />","⟨f ⟨<//> ⟨+ a b⟩⟩⟩")

  , ("[ a | b ]","⟨[|] a b⟩")

  , ("2","2")
  ]

checkTests = mapM_ check tests

check (i,o) = case parse i of
  S [] [] [[o']] Success ->
    if o == show o'
    then return ()
    else do putStrLn $ "FAIL: input: " ++ i
              ++ ", expected: " ++ o
              ++ ", computed: " ++ show o'
            steps i
  _ -> do putStrLn $ "FAIL: input: " ++ i
            ++ ", expected: " ++ o
            ++ ", computed: Nothing."
          steps i
                           

