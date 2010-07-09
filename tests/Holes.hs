module Holes where

import Text.Syntactical
import Text.Syntactical.Data

table :: Table String
table = buildTable
 [ [ closed_ "(" [] ")" Distfix
   , closed_ "⟨" [] "⟩" SExpression
   , closed "</" []"/>" Distfix
   , closed "{" [] "}" SExpression
   , Op2 True "[" [] Distfix "]"
   , Op2 True "[" [(Distfix, "|")] Distfix "]"
   , Op2 True "[" [(Distfix, "|"),(Distfix, "|")] Distfix "]"
   , Op2 True "[." [(Distfix, "|")] Distfix "]"
   , Op2 True "</" [(Distfix, "|")] Distfix "/>"
   , Op2 True "|" [] Distfix "|"
   ]
 ]

-- [(input, expected output)]
tests :: [(String,String)]
tests =
  [ ("1", "1")
  , ("(1)", "1")
  , ("((1))", "1")
  , ("⟨1⟩", "⟨1⟩")
  , ("⟨⟨1⟩⟩", "⟨⟨1⟩⟩")

  , ("a", "a")
  , ("(a)", "a")
  , ("((a))", "a")
  , ("⟨a⟩", "⟨a⟩")
  , ("⟨⟨a⟩⟩", "⟨⟨a⟩⟩")

  , ("f a", "⟨f a⟩")
  , ("(f a)", "⟨f a⟩")
  , ("((f a))", "⟨f a⟩")
  , ("⟨⟨f a⟩⟩", "⟨⟨f a⟩⟩")

  , ("</ 1 />", "⟨<//> 1⟩")
  , ("</ </ 1 /> />", "⟨<//> ⟨<//> 1⟩⟩")

  , ("</ a />", "⟨<//> a⟩")
  , ("</ </ a /> />", "⟨<//> ⟨<//> a⟩⟩")

  , ("{ a }", "⟨{} ⟨a⟩⟩")         -- TODO Not ⟨{} a⟩; put this in the documentation.
  , ("{ a b }", "⟨{} ⟨a b⟩⟩")     -- It seems unintuitive, but it is consistent as
  , ("{ a b c }", "⟨{} ⟨a b c⟩⟩") -- it allows list of one, two, and more elements.
  , ("{ 1 }", "⟨{} ⟨1⟩⟩")
  , ("{ 1 2 }", "⟨{} ⟨1 2⟩⟩")
  , ("{ 1 2 3 }", "⟨{} ⟨1 2 3⟩⟩")
  , ("{ + 1 2 }", "⟨{} ⟨+ 1 2⟩⟩")
  , ("{ 1 + 2 }", "⟨{} ⟨1 + 2⟩⟩")
  , ("{ 1 2 + }", "⟨{} ⟨1 2 +⟩⟩")

  , ("[ a ]", "⟨[] a⟩")
  , ("[ a | b ]", "⟨[|] a b⟩")
  , ("[ a | b | c ]", "⟨[||] a b c⟩")
  , ("[. a | b ]", "⟨[.|] a b⟩")

  , ("</ a | b />", "⟨</|/> a b⟩")

  , ("| a |", "⟨|| a⟩")
  , ("[ (| a |) ]", "⟨[] ⟨|| a⟩⟩")

  , ("⟨⟩", "⟨⟩")
  , ("⟨⟩ a", "⟨⟨⟩ a⟩")
  , ("f ⟨⟩", "⟨f ⟨⟩⟩")
  , ("f ⟨⟩ 1", "⟨f ⟨⟩ 1⟩")

  ]

