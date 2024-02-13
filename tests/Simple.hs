module Simple where

import Data.String
import Protolude hiding (head, Associativity, First, Infix, LeftAssociative, Prefix, Last, RightAssociative)
import System.Environment (getArgs)
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Text.Syntactical
import Text.Syntactical.Data

table :: Table String
table = buildTable
 [ [ closed_ "(" Distfix ")"
   , closed_ "⟨" SExpression "⟩"
   , closed "</" Distfix"/>"
   , closed "[" Distfix "|" `distfix` "]"
   , closed "{" Distfix "}"
   ]
 , [ infx RightAssociative "?'" `distfix` ":'"
   , postfx "!"
   , postfx "_/" `distfix` "/."
   ]
 , [ postfx "%"
   , prefx "#"
   ]
 , [ postfx "°"
   , infx LeftAssociative "*"
   , infx LeftAssociative "/"
   ]
 , [ infx LeftAssociative "+"
   , infx LeftAssociative "-"
   ]
 , [ infx LeftAssociative "<<"
   , infx LeftAssociative ">>"
   , infx RightAssociative "?" `distfix` ":"
   ]
 , [ prefx "if" `distfix` "then" `distfix` "else"
   ]
 , [ infx RightAssociative ","
   ]
 , [ Op1 True "\\" [(SExpression,"->")] Prefix 0
   ]
 , [ prefx "let" `distfix` "in"
   , infx RightAssociative "where"
   , prefx "case" `distfix` "of"
   ]
 , [ infx NonAssociative "="
   ]
 , [ infx RightAssociative ";"
   ]
 , [ infx RightAssociative "::" `distfix` ";;"
   , infx RightAssociative "==" `distfix` ";;"
   ]
 ]

-- [(input, expected output)]
tests :: [(String,String)]
tests = [
  ("1","1"),
  ("a","a"),

  ("1 + 2","⟨␣+␣ 1 2⟩"),
  ("a + 2","⟨␣+␣ a 2⟩"),
  ("1 + b","⟨␣+␣ 1 b⟩"),
  ("a + b","⟨␣+␣ a b⟩"),
  ("1 * 2","⟨␣*␣ 1 2⟩"),

  ("1 + 2 + 3","⟨␣+␣ ⟨␣+␣ 1 2⟩ 3⟩"),
  ("1 + 2 * 3","⟨␣+␣ 1 ⟨␣*␣ 2 3⟩⟩"),
  ("1 * 2 + 3","⟨␣+␣ ⟨␣*␣ 1 2⟩ 3⟩")

  , ("0 << 1 + 2 * 3", "⟨␣<<␣ 0 ⟨␣+␣ 1 ⟨␣*␣ 2 3⟩⟩⟩")
  , ("0 + 1 << 2 * 3", "⟨␣<<␣ ⟨␣+␣ 0 1⟩ ⟨␣*␣ 2 3⟩⟩")
  , ("0 + 1 * 2 << 3", "⟨␣<<␣ ⟨␣+␣ 0 ⟨␣*␣ 1 2⟩⟩ 3⟩")
  , ("0 << 1 * 2 + 3", "⟨␣<<␣ 0 ⟨␣+␣ ⟨␣*␣ 1 2⟩ 3⟩⟩")
  , ("0 * 1 << 2 + 3", "⟨␣<<␣ ⟨␣*␣ 0 1⟩ ⟨␣+␣ 2 3⟩⟩")
  , ("0 * 1 + 2 << 3", "⟨␣<<␣ ⟨␣+␣ ⟨␣*␣ 0 1⟩ 2⟩ 3⟩")
  , ("(0 << 1) + 2 * 3", "⟨␣+␣ ⟨␣<<␣ 0 1⟩ ⟨␣*␣ 2 3⟩⟩")
  , ("0 << (1 + 2) * 3", "⟨␣<<␣ 0 ⟨␣*␣ ⟨␣+␣ 1 2⟩ 3⟩⟩")
  , ("0 << 1 + (2 * 3)", "⟨␣<<␣ 0 ⟨␣+␣ 1 ⟨␣*␣ 2 3⟩⟩⟩")
  , ("((0 << 1) + 2) * 3", "⟨␣*␣ ⟨␣+␣ ⟨␣<<␣ 0 1⟩ 2⟩ 3⟩")
  , ("(((0 << 1) + 2) * 3)", "⟨␣*␣ ⟨␣+␣ ⟨␣<<␣ 0 1⟩ 2⟩ 3⟩")
  , ("⟨<< 0 1⟩ + 2 * 3", "⟨␣+␣ ⟨<< 0 1⟩ ⟨␣*␣ 2 3⟩⟩")
  , ("⟨+ ⟨<< 0 1⟩ 2⟩ * 3", "⟨␣*␣ ⟨+ ⟨<< 0 1⟩ 2⟩ 3⟩")

  , ("f a","⟨f a⟩"),
  ("f 1","⟨f 1⟩"),
  ("f a b","⟨f a b⟩"),
  ("f 1 b","⟨f 1 b⟩"),
  ("f a 1","⟨f a 1⟩"),
  ("f 1 2","⟨f 1 2⟩"),
  ("f 1 2 3","⟨f 1 2 3⟩"),

  ("f a + 1","⟨␣+␣ ⟨f a⟩ 1⟩"),
  ("1 + f a","⟨␣+␣ 1 ⟨f a⟩⟩"),

  ("(a)","a"),
  ("((a))","a"),
  ("1 + (a)","⟨␣+␣ 1 a⟩"),
  ("1 + ((a))","⟨␣+␣ 1 a⟩"),
  ("(1 + 2)","⟨␣+␣ 1 2⟩"),
  ("(1 + (a))","⟨␣+␣ 1 a⟩"),
  ("(1 + ((a)))","⟨␣+␣ 1 a⟩"),

  ("1 * (2 + 3)","⟨␣*␣ 1 ⟨␣+␣ 2 3⟩⟩"),
  ("(1 + 2) * 3","⟨␣*␣ ⟨␣+␣ 1 2⟩ 3⟩"),
  ("1 + (f a)","⟨␣+␣ 1 ⟨f a⟩⟩"),
  ("(f a) + 1","⟨␣+␣ ⟨f a⟩ 1⟩"),
  ("(f a b) 1","⟨⟨f a b⟩ 1⟩"),
  ("(f a b) 1 2","⟨⟨f a b⟩ 1 2⟩"),
  ("1 + (f a) 2","⟨␣+␣ 1 ⟨⟨f a⟩ 2⟩⟩")
  , ("f (a + b) (1 - 2)", "⟨f ⟨␣+␣ a b⟩ ⟨␣-␣ 1 2⟩⟩")

  , ("⟨1⟩", "⟨1⟩")
  , ("⟨a⟩", "⟨a⟩")
  , ("⟨⟨1⟩⟩", "⟨⟨1⟩⟩")
  , ("⟨⟨a⟩⟩", "⟨⟨a⟩⟩")
  , ("⟨+ a b⟩", "⟨+ a b⟩")
  , ("⟨+ a b⟩ * (1 - 2)", "⟨␣*␣ ⟨+ a b⟩ ⟨␣-␣ 1 2⟩⟩")
  , ("(a + b) * ⟨- 1 2⟩", "⟨␣*␣ ⟨␣+␣ a b⟩ ⟨- 1 2⟩⟩")
  , ("⟨␣*␣ (a + b) (1 - 2)⟩", "⟨␣*␣ ⟨␣+␣ a b⟩ ⟨␣-␣ 1 2⟩⟩")
  , ("⟨␣*␣ (a + b) ⟨- 1 2⟩⟩", "⟨␣*␣ ⟨␣+␣ a b⟩ ⟨- 1 2⟩⟩")

  , ("true ? 1 : 0", "⟨␣?␣:␣ true 1 0⟩")
  , ("true ? 1 : 0 + 1", "⟨␣?␣:␣ true 1 ⟨␣+␣ 0 1⟩⟩")
  , ("true ?' 1 :' 0 + 1", "⟨␣+␣ ⟨␣?'␣:'␣ true 1 0⟩ 1⟩")

  , ("# a", "⟨#␣ a⟩")
  , ("a # b", "⟨a ⟨#␣ b⟩⟩")
  , ("# # a", "⟨#␣ ⟨#␣ a⟩⟩")

  , ("a !", "⟨␣! a⟩")
  , ("a ! b", "⟨⟨␣! a⟩ b⟩")
  , ("a ! !", "⟨␣! ⟨␣! a⟩⟩")

  , ("# a °", "⟨␣° ⟨#␣ a⟩⟩")
  , ("# a !", "⟨#␣ ⟨␣! a⟩⟩")
  , ("a ! # b", "⟨⟨␣! a⟩ ⟨#␣ b⟩⟩")

  , ("1 + # b", "⟨␣+␣ 1 ⟨#␣ b⟩⟩")
  , ("# b 1", "⟨#␣ ⟨b 1⟩⟩")
  , ("b 1 !", "⟨␣! ⟨b 1⟩⟩")

  , ("if true then 1 else 0", "⟨if␣then␣else␣ true 1 0⟩")
  , ("if 2 then 1 else 0", "⟨if␣then␣else␣ 2 1 0⟩")
  , ("if a b then 1 else 0", "⟨if␣then␣else␣ ⟨a b⟩ 1 0⟩")
  , ("if true then a b else 0", "⟨if␣then␣else␣ true ⟨a b⟩ 0⟩")
  , ("if true then 1 else a b", "⟨if␣then␣else␣ true 1 ⟨a b⟩⟩")
  , ("1 + if true then 1 else 0", "⟨␣+␣ 1 ⟨if␣then␣else␣ true 1 0⟩⟩")
  , ("1 + if true then 1 else a b + c", "⟨␣+␣ 1 ⟨if␣then␣else␣ true 1 ⟨␣+␣ ⟨a b⟩ c⟩⟩⟩")
  , ("f if true then 1 else 0", "⟨f ⟨if␣then␣else␣ true 1 0⟩⟩")
  , ("true ? 1 : if true then 1 else 0", "⟨␣?␣:␣ true 1 ⟨if␣then␣else␣ true 1 0⟩⟩")

  , ("</ a />","⟨</␣/> a⟩")
  , ("</ 0 />","⟨</␣/> 0⟩")
  , ("</ f a b />","⟨</␣/> ⟨f a b⟩⟩")
  , ("</ f 1 2 />","⟨</␣/> ⟨f 1 2⟩⟩")
  , ("</ a + b />","⟨</␣/> ⟨␣+␣ a b⟩⟩")
  , ("</ a + b * 2 />","⟨</␣/> ⟨␣+␣ a ⟨␣*␣ b 2⟩⟩⟩")
  , ("</ a /> + 1","⟨␣+␣ ⟨</␣/> a⟩ 1⟩")
  , ("1 + </ a />","⟨␣+␣ 1 ⟨</␣/> a⟩⟩")
  , ("1 + </ a - b /> * 2","⟨␣+␣ 1 ⟨␣*␣ ⟨</␣/> ⟨␣-␣ a b⟩⟩ 2⟩⟩")
  , ("</ a + b /> c","⟨⟨</␣/> ⟨␣+␣ a b⟩⟩ c⟩")
  , ("f </ a />","⟨f ⟨</␣/> a⟩⟩")
  , ("f </ a + b />","⟨f ⟨</␣/> ⟨␣+␣ a b⟩⟩⟩")

  , ("</ # 0 />","⟨</␣/> ⟨#␣ 0⟩⟩")
  , ("</ 1 + # 0 />","⟨</␣/> ⟨␣+␣ 1 ⟨#␣ 0⟩⟩⟩")

  , ("[ a | b ]","⟨[␣|␣] a b⟩")

  , ("a = b ; c = d", "⟨␣;␣ ⟨␣=␣ a b⟩ ⟨␣=␣ c d⟩⟩")
  , ("a = let { b = c } in b", "⟨␣=␣ a ⟨let␣in␣ ⟨{␣} ⟨␣=␣ b c⟩⟩ b⟩⟩")
  , ("a = let { b } in b ; d", "⟨␣;␣ ⟨␣=␣ a ⟨let␣in␣ ⟨{␣} b⟩ b⟩⟩ d⟩")
  , ("a = let { b = c } in b ; d = e",
     "⟨␣;␣ ⟨␣=␣ a ⟨let␣in␣ ⟨{␣} ⟨␣=␣ b c⟩⟩ b⟩⟩ ⟨␣=␣ d e⟩⟩")
  , ("a = let { b = c ; f = g } in b ; d = e",
     "⟨␣;␣ ⟨␣=␣ a ⟨let␣in␣ ⟨{␣} ⟨␣;␣ ⟨␣=␣ b c⟩ ⟨␣=␣ f g⟩⟩⟩ b⟩⟩ ⟨␣=␣ d e⟩⟩")
  , ("a = b where { c } ; d", "⟨␣;␣ ⟨␣=␣ a ⟨␣where␣ b ⟨{␣} c⟩⟩⟩ d⟩")

  , ("f a b = let { c } in case d of { e }",
     "⟨␣=␣ ⟨f a b⟩ ⟨let␣in␣ ⟨{␣} c⟩ ⟨case␣of␣ d ⟨{␣} e⟩⟩⟩⟩")

  , ("# true ? 1 : 0", "⟨␣?␣:␣ ⟨#␣ true⟩ 1 0⟩")

  , ("a _/ b /.", "⟨␣_/␣/. a b⟩")
  , ("a _/ 1 + 2 /.", "⟨␣_/␣/. a ⟨␣+␣ 1 2⟩⟩")
  , ("a _/ (b) /.", "⟨␣_/␣/. a b⟩")
  , ("a _/ 1 + 2 /. b", "⟨⟨␣_/␣/. a ⟨␣+␣ 1 2⟩⟩ b⟩")
  , ("a + b _/ c /.", "⟨␣+␣ a ⟨␣_/␣/. b c⟩⟩")
  , ("a + b * c _/ 1 + 2 /.", "⟨␣+␣ a ⟨␣*␣ b ⟨␣_/␣/. c ⟨␣+␣ 1 2⟩⟩⟩⟩")

  , ("if true then if true then 1 else 0 else 2",
     "⟨if␣then␣else␣ true ⟨if␣then␣else␣ true 1 0⟩ 2⟩")
  , ("if true then 1 else # 0", "⟨if␣then␣else␣ true 1 ⟨#␣ 0⟩⟩")
  , ("if true then # 1 else 0", "⟨if␣then␣else␣ true ⟨#␣ 1⟩ 0⟩")
  , ("# if true then 1 else 0", "⟨#␣ ⟨if␣then␣else␣ true 1 0⟩⟩")
  , ("[ a + b | c ]", "⟨[␣|␣] ⟨␣+␣ a b⟩ c⟩")
  , ("[ a | b + c ]", "⟨[␣|␣] a ⟨␣+␣ b c⟩⟩")
  , ("[ a + b | c * d ]", "⟨[␣|␣] ⟨␣+␣ a b⟩ ⟨␣*␣ c d⟩⟩")

  , ("⟨⟩", "⟨⟩")
  , ("⟨⟩ a", "⟨⟨⟩ a⟩")
  , ("f ⟨⟩ 1", "⟨f ⟨⟩ 1⟩")

  , ("\\ a b -> a + b", "⟨\\␣->␣ ⟨a b⟩ ⟨␣+␣ a b⟩⟩")
  , ("\\ a 2 -> a + b", "⟨\\␣->␣ ⟨a 2⟩ ⟨␣+␣ a b⟩⟩")
  , ("\\ 1 2 -> a + b", "⟨\\␣->␣ ⟨1 2⟩ ⟨␣+␣ a b⟩⟩")

-- See the Note in Yard.hs.
  , ("1 2", "⟨1 2⟩")
  , ("(1 2)", "⟨1 2⟩")
  , ("f (1 2)", "⟨f ⟨1 2⟩⟩")
  , ("</ 1 2 />", "⟨</␣/> ⟨1 2⟩⟩")
  , ("1 2 + 3", "⟨␣+␣ ⟨1 2⟩ 3⟩")
  , ("1 (1 + 2)", "⟨1 ⟨␣+␣ 1 2⟩⟩")
  , ("1 a", "⟨1 a⟩")
  ]

tests' :: [(String, Failure String)]
tests' =
  [ ("true then 1 else 0", MissingBefore [["if"]] "then")
  , ("if true 1 else 0", MissingBefore [["if","then"]] "else")
  , ("true 1 else 0", MissingBefore [["if","then"]] "else")
  , ("if true then 1", MissingAfter ["else"] ["if","then"])
  , ("[ a | b", MissingAfter ["]"] ["[","|"])
  , ("a | b ]", MissingBefore [["["]] "|")
  , ("[ a b ]", MissingBefore [["[","|"]] "]")
  , ("1 + * 3", MissingSubBetween "+" "*")
  , ("1 * + 3", MissingSubBetween "*" "+")
  , ("()", MissingSubBetween "(" ")")
  , ("[ | b ]", MissingSubBetween "[" "|")
  , ("[ a | ]", MissingSubBetween "|" "]")
  , ("true ? 1 : true then 1 else 0", MissingBefore [["if"]] "then")
  , ("true ? 1 : then 1 else 0", MissingBefore [["if"]] "then")
  , ("a _/ /.", MissingSubBetween "_/" "/.")
  , ("a _/ b", MissingAfter ["/."] ["_/"])
  , ("(+ 2)", MissingSubBetween "(" "+")
  , ("true : 1 + 2", MissingBefore [["?"]] ":")
  , ("+ 1 2", MissingSubBefore "+")
  , ("+", MissingSubBefore "+")
  , ("+ 1", MissingSubBefore "+")
  , ("1 +", MissingSubAfter "+")
  , ("|", MissingBefore [["["]] "|")
  , ("[", MissingAfter ["|"] ["["])
  , ("]", MissingBefore [["[","|"]] "]")
  , ("(", MissingAfter [")"] ["("])
  , (")", MissingBefore [["("]] ")")
  , ("⟨", MissingAfter ["⟩"] ["⟨"])
  , ("⟩", MissingBefore [["⟨"]] "⟩")
  , ("_/ b /.", MissingSubBefore "_/")
  ]

