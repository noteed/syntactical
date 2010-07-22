module Ambiguous where

import Text.Syntactical
import Text.Syntactical.Data

table :: Table String
table = buildTable
 [ [ closed "<" Distfix "|"
   , closed "<" Distfix "|" `distfix` ">"
   , infx LeftAssociative "?"
   , infx LeftAssociative "?" `distfix` ":"
   , prefx "!"
   , postfx "!"
   , prefx "<-" `distfix` "--"
   , postfx "<-" `distfix` "++"
   , prefx ">-" `distfix` "--"
   , prefx ">-" `sexpr` "++"
   , prefx ">>" `distfix` "//" `distfix` ".."
   , prefx ">>" `distfix` "//" `sexpr` ";;"
   ]
 ]

tests :: [(String,Failure String)]
tests =
  [ ("< 1 |", Ambiguity MiddleOrLast)
  , ("< 1 | 2", Ambiguity MiddleOrLast)
  , ("1 ? 2", Ambiguity LoneOrFirst)
  , ("! 1", Ambiguity MultipleLone)
  , ("<- 1 -- 2", Ambiguity NotSameFirst)
  , ("1 <- 2 --", Ambiguity NotSameFirst)
  , (">- 1 -- 2", Ambiguity NotSameHole)
  , (">- 1 ++ 2", Ambiguity NotSameHole)
  , (">> 1 // 2 .. 3", Ambiguity NotSameHole)
  , (">> 1 // 2 ;; 3", Ambiguity NotSameHole)
  ]

