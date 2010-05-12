module Language.Syntactical.Tests.Examples where

import Language.Syntactical.Yard

someTable :: [Op]
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

checkTests = mapM_ (check someTable) tests

check table (i,o) = case parse table i of
  S [] [] [[o']] Success ->
    if o == show o'
    then return ()
    else do putStrLn $ "FAIL: input: " ++ i
              ++ ", expected: " ++ o
              ++ ", computed: " ++ show o'
            steps table i
  _ -> do putStrLn $ "FAIL: input: " ++ i
            ++ ", expected: " ++ o
            ++ ", computed: Nothing."
          steps table i

