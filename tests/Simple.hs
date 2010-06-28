module Main where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import System.Environment (getArgs)

import Text.Syntactical
import Text.Syntactical.Data

table0 :: Table
table0 = buildTable
 [ [ closed_ "(" [] ")" Distfix
   , closed_ "⟨" [] "⟩" SExpression
   , closed "</" []"/>" Distfix
   , closed "[" ["|"] "]" Distfix
   , closed "{" [] "}" Distfix
   ]
 , [ infx "?'" [":'"] RightAssociative
   , postfx "!" []
   , postfx "_/" ["/."]
   ]
 , [ postfx "%" []
   , prefx "#" []
   ]
 , [ postfx "°" []
   , infx "*" [] LeftAssociative
   , infx "/" [] LeftAssociative
   ]
 , [ infx "+" [] LeftAssociative
   , infx "-" [] LeftAssociative
   ]
 , [ infx "<<" [] LeftAssociative
   , infx ">>" [] LeftAssociative
   , infx "?" [":"] RightAssociative
   ]
 , [ prefx "if" ["then","else"]
   ]
 , [ infx "," [] RightAssociative
   ]
 , [ Op1 True "\\" [(SExpression,"->")] (RightOpen True) RightAssociative 0
   ]
 , [ prefx "let" ["in"]
   , infx "where" [] RightAssociative
   , prefx "case" ["of"]
   ]
 , [ infx "=" [] NonAssociative
   ]
 , [ infx ";" [] RightAssociative
   ]
 , [ infx "::" [";;"] RightAssociative
   , infx "==" [";;"] RightAssociative
   ]
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
  , ("# a !", "⟨# ⟨! a⟩⟩")
  , ("a ! # b", "⟨⟨! a⟩ ⟨# b⟩⟩")

  , ("1 + # b", "⟨+ 1 ⟨# b⟩⟩")
  , ("# b 1", "⟨# ⟨b 1⟩⟩")
  , ("b 1 !", "⟨! ⟨b 1⟩⟩")

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

  , ("</ # 0 />","⟨<//> ⟨# 0⟩⟩")
  , ("</ 1 + # 0 />","⟨<//> ⟨+ 1 ⟨# 0⟩⟩⟩")

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
  , ("a _/ (b) /.", "⟨_//. a b⟩")
  , ("a _/ 1 + 2 /. b", "⟨⟨_//. a ⟨+ 1 2⟩⟩ b⟩")
  , ("a + b _/ c /.", "⟨+ a ⟨_//. b c⟩⟩")
  , ("a + b * c _/ 1 + 2 /.", "⟨+ a ⟨* b ⟨_//. c ⟨+ 1 2⟩⟩⟩⟩") -- TODO prec postfix < prec *

  , ("if true then if true then 1 else 0 else 2",
     "⟨ifthenelse true ⟨ifthenelse true 1 0⟩ 2⟩")
  , ("if true then 1 else # 0", "⟨ifthenelse true 1 ⟨# 0⟩⟩")
  , ("if true then # 1 else 0", "⟨ifthenelse true ⟨# 1⟩ 0⟩")
  , ("# if true then 1 else 0", "⟨# ⟨ifthenelse true 1 0⟩⟩")
  , ("[ a + b | c ]", "⟨[|] ⟨+ a b⟩ c⟩")
  , ("[ a | b + c ]", "⟨[|] a ⟨+ b c⟩⟩")
  , ("[ a + b | c * d ]", "⟨[|] ⟨+ a b⟩ ⟨* c d⟩⟩")

  , ("⟨⟩", "⟨⟩")
  , ("⟨⟩ a", "⟨⟨⟩ a⟩")
  , ("f ⟨⟩ 1", "⟨f ⟨⟩ 1⟩")

  , ("\\ a b -> a + b", "⟨\\-> ⟨a b⟩ ⟨+ a b⟩⟩")
  , ("\\ a 2 -> a + b", "⟨\\-> ⟨a 2⟩ ⟨+ a b⟩⟩")
  , ("\\ 1 2 -> a + b", "⟨\\-> ⟨1 2⟩ ⟨+ a b⟩⟩")
  ]

testsTable0' :: [(String, Failure)]
testsTable0' =
  [ ("true then 1 else 0", MissingBefore ["if"] "then")
  , ("if true 1 else 0", Incomplete ["if"]) -- MissingBefore ["then"] "else")
  , ("true 1 else 0", MissingBefore ["then"] "else")
  , ("if true then 1", MissingAfter ["else"] ["if","then"])
  , ("[ a | b", MissingAfter ["]"] ["[","|"])
  , ("a | b ]", MissingBefore ["["] "|")
  , ("[ a b ]", Incomplete ["["]) -- MissingBefore ["|"] "]")
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
  , ("true ? 1 : then 1 else 0", EmptyHole ":" "then") -- MissingBefore ["if"] "then")
  , ("a _/ /.", EmptyHole "_/" "/.")
  , ("a _/ b", MissingAfter ["/."] ["_/"])
  , ("(+ 2)", EmptyHole "(" "+")
  , ("true : 1 + 2", MissingBefore ["?"] ":")
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
  , ("(", )
  , (")", )
  , ("⟨", )
  , ("⟩", )
  , ("1 (1 + 2)", ) -- The equation with a number on the output stack should be extended.
  , ("# a %", Error "precedence cannot be mixed")
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

token (c:cs) | c `elem` ['a'..'z'] ++ "()⟨⟩+-*/?:#i°%!<>[]|,{};=\"._\\" = Sym (c:cs)
             | otherwise = Num (read [c])

-- 

main :: IO ()
main = defaultMain
  [ testYard
  ]

testYard :: Test
testYard = testGroup "Text.Syntactical.Yard"
  [ testGroup "Text.Syntactical.Tests.Examples - table0" $
    map (helper parse0) testsTable0
  , testGroup "Text.Syntactical.Tests.Examples - table0 - bad input" $
    map (helper' parse0) testsTable0'
  ]

-- Apply the parser p to i and check if it returns
-- the expected value o.
helper p (i,o) = testCase i $ case p i of
  Right o' -> o @=? show o'
  Left err -> assertFailure $ "cannot parse: " ++ show err

helper' p (i,o) = testCase i $ case p i of
  Right o' -> assertFailure $ "unexpected successful parse: " ++ show o'
  Left o' -> o @=? o'

