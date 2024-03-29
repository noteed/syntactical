module Holes where

import Data.String
import Protolude hiding (head, Associativity, First, Infix, LeftAssociative, Prefix, Last)
import Text.Syntactical
import Text.Syntactical.Data

table :: Table String
table = buildTable
 [ [ closed_ "(" Distfix ")"
   , closed_ "⟨" SExpression "⟩"
   , closed "</" Distfix "/>"
   , closed "{" SExpression "}"
   , closed "{" SExpression "|" `distfix` "}"
   , closed "[" Distfix "]"
   , closed "[" Distfix "|" `distfix` "]"
   , closed "[" Distfix "|" `distfix` "|" `distfix` "]"
   , closed "[." Distfix "|" `distfix` "]"
   , closed "</" Distfix "|" `distfix` "/>"
   , closed "|" Distfix "|"
   ]
 , [ infx LeftAssociative "+"
   , infx LeftAssociative "-"
   ]
 ]

-- [(input, expected output)]
tests :: [(String, String)]
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

  , ("</ 1 />", "⟨</␣/> 1⟩")
  , ("</ </ 1 /> />", "⟨</␣/> ⟨</␣/> 1⟩⟩")

  , ("</ a />", "⟨</␣/> a⟩")
  , ("</ </ a /> />", "⟨</␣/> ⟨</␣/> a⟩⟩")

  , ("{ a }", "⟨{␣} ⟨a⟩⟩")
  , ("{ a b }", "⟨{␣} ⟨a b⟩⟩")
  , ("{ a b c }", "⟨{␣} ⟨a b c⟩⟩")
  , ("{ 1 }", "⟨{␣} ⟨1⟩⟩")
  , ("{ 1 2 }", "⟨{␣} ⟨1 2⟩⟩")
  , ("{ 1 2 3 }", "⟨{␣} ⟨1 2 3⟩⟩")
  , ("{ + 1 2 }", "⟨{␣} ⟨+ 1 2⟩⟩")
  , ("{ 1 + 2 }", "⟨{␣} ⟨1 + 2⟩⟩")
  , ("{ 1 2 + }", "⟨{␣} ⟨1 2 +⟩⟩")

  , ("{ 1 + 2 | a - b }", "⟨{␣|␣} ⟨1 + 2⟩ ⟨␣-␣ a b⟩⟩")

  , ("[ a ]", "⟨[␣] a⟩")
  , ("[ a | b ]", "⟨[␣|␣] a b⟩")
  , ("[ a | b | c ]", "⟨[␣|␣|␣] a b c⟩")
  , ("[. a | b ]", "⟨[.␣|␣] a b⟩")

  , ("</ a | b />", "⟨</␣|␣/> a b⟩")

  , ("| a |", "⟨|␣| a⟩")
  , ("[ (| a |) ]", "⟨[␣] ⟨|␣| a⟩⟩")

  , ("⟨⟩", "⟨⟩")
  , ("⟨⟩ a", "⟨⟨⟩ a⟩")
  , ("f ⟨⟩", "⟨f ⟨⟩⟩")
  , ("f ⟨⟩ 1", "⟨f ⟨⟩ 1⟩")

  ]

