module Text.Syntactical.Tests.Examples where

import Text.Syntactical
import Text.Syntactical.Yard (Done(..))

table0 :: Table
table0 = Table
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
 , Postfix [] ["_/","/."] 9
 , Prefix [] ["if","then","else"] 3
 , Closed [] ["</","/>"] Keep
 , Closed [] ["[","|","]"] Keep
 , Infix [] [","] RightAssociative 2
 , Closed [] ["{","}"] Keep
 , Infix [] [";"] RightAssociative (-2)
 , Infix [] ["="] NonAssociative (-1)
 , Prefix [] ["let","in"] 0
 , Infix [] ["where"] RightAssociative 0
 , Prefix [] ["case","of"] 0
 ]

-- [(input, expected output)]
testsTable0 :: [(String,String)]
testsTable0 = [
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
  ("f 1 2","⟨f 1 2⟩"),
  ("f 1 2 3","⟨f 1 2 3⟩"),

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
  , ("a ! # b", "⟨⟨! a⟩ ⟨# b⟩⟩")

  , ("if true then 1 else 0", "⟨ifthenelse true 1 0⟩")
  , ("if 2 then 1 else 0", "⟨ifthenelse 2 1 0⟩")
  , ("if a b then 1 else 0", "⟨ifthenelse ⟨a b⟩ 1 0⟩")
  , ("if true then a b else 0", "⟨ifthenelse true ⟨a b⟩ 0⟩")
  , ("if true then 1 else a b", "⟨ifthenelse true 1 ⟨a b⟩⟩")
  , ("1 + if true then 1 else 0", "⟨+ 1 ⟨ifthenelse true 1 0⟩⟩")
  , ("1 + if true then 1 else a b + c", "⟨+ 1 ⟨ifthenelse true 1 ⟨+ ⟨a b⟩ c⟩⟩⟩")
  , ("f if true then 1 else 0", "⟨f ⟨ifthenelse true 1 0⟩⟩")
  , ("true ? 1 : if true then 1 else 0", "⟨?: true 1 ⟨ifthenelse true 1 0⟩⟩")

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

  , ("a = b ; c = d", "⟨; ⟨= a b⟩ ⟨= c d⟩⟩")
  , ("a = let { b = c } in b", "⟨= a ⟨letin ⟨{} ⟨= b c⟩⟩ b⟩⟩")
  , ("a = let { b } in b ; d", "⟨; ⟨= a ⟨letin ⟨{} b⟩ b⟩⟩ d⟩")
  , ("a = let { b = c } in b ; d = e",
     "⟨; ⟨= a ⟨letin ⟨{} ⟨= b c⟩⟩ b⟩⟩ ⟨= d e⟩⟩")
  , ("a = let { b = c ; f = g } in b ; d = e",
     "⟨; ⟨= a ⟨letin ⟨{} ⟨; ⟨= b c⟩ ⟨= f g⟩⟩⟩ b⟩⟩ ⟨= d e⟩⟩")
  , ("a = b where { c } ; d", "⟨; ⟨= a ⟨where b ⟨{} c⟩⟩⟩ d⟩")

  , ("f a b = let { c } in case d of { e }",
     "⟨= ⟨f a b⟩ ⟨letin ⟨{} c⟩ ⟨caseof d ⟨{} e⟩⟩⟩⟩")

  , ("# true ? 1 : 0", "⟨?: ⟨# true⟩ 1 0⟩")

  , ("a _/ b /.", "⟨_//. a b⟩")
  , ("a _/ 1 + 2 /.", "⟨_//. a ⟨+ 1 2⟩⟩")

  -- TODO , ("⟨⟩", "⟨⟩")
  ]

testsTable0' :: [(String, Done)]
testsTable0' =
  [ ("true then 1 else 0", MissingBefore ["if"] "then")
  , ("if true 1 else 0", MissingBefore ["then"] "else")
  , ("true 1 else 0", MissingBefore ["if","then"] "else")
  , ("if true then 1", MissingAfter "else" ["if","then"])
  , ("[ a | b", MissingAfter "]" ["[","|"])
  , ("a | b ]", MissingBefore ["["] "|")
  , ("[ a b ]", MissingBefore ["|"] "]")
  , ("[ a b ]", MissingBefore ["|"] "]")
  , ("1 2", CantApply 1 2)
  , ("(1 2)", CantApply 1 2)
  , ("f (1 2)", CantApply 1 2)
  , ("</ 1 2 />", CantApply 1 2)
  , ("1 2 + 3", CantApply 1 2)
  , ("1 + * 3", EmptyHole "+" "*")
  , ("1 * + 3", EmptyHole "*" "+")
  , ("()", EmptyHole "(" ")")
  , ("[ | b ]", EmptyHole "[" "|")
  , ("[ a | ]", EmptyHole "|" "]")
  , ("true ? 1 : true then 1 else 0", MissingBefore ["if"] "then")
  , ("true ? 1 : then 1 else 0", MissingBefore ["if"] "then")
  , ("a _/ /.", EmptyHole "_/" "/.")
  , ("a _/ b", MissingAfter "/." ["_/"])
-- TODO cases above with parenthesis or in bigger expression.
-- TODO obviously those are not success, but I have to
-- create and recognize the error cases.
{-
  , ("1 +", Success)
  , ("+ 1 2", Success)
  , ("+ 2", Success)
  , ("+", Success)
  , ("|", Success)
  , ("[", Success)
  , ("]", Success)
  , ("_/ b /.", Success)
-}
  ]

checkTable0 = checkTests table0 testsTable0

parse0 = shunt table0 . tokenize

steps0 = steps table0 . tokenize

checkTests table l = mapM_ (check table) l

check table (i,o) =
  let ts = tokenize i in case shunt table ts of
  Right o' ->
    if o == show o'
    then return ()
    else do putStrLn $ "FAIL: input: " ++ i
              ++ ", expected: " ++ o
              ++ ", computed: " ++ show o'
            steps table ts
  _ -> do putStrLn $ "FAIL: input: " ++ i
            ++ ", expected: " ++ o
            ++ ", computed: Nothing."
          steps table ts

tokenize = map (token) . separate

separate = words . separate'
separate' ('(':cs) = " ( " ++ separate' cs
separate' (')':cs) = " ) " ++ separate' cs
separate' ('⟨':cs) = " ⟨ " ++ separate' cs
separate' ('⟩':cs) = " ⟩ " ++ separate' cs
separate' (c:cs) = c : separate' cs
separate' [] = []

token (c:cs) | c `elem` ['a'..'z'] ++ "()⟨⟩+-*/?:#i°%!<>[]|,{};=\"._" = Sym (c:cs)
             | otherwise = Num (read [c])

