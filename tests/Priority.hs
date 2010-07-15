module Priority where

import Text.Syntactical
import Text.Syntactical.Data

table :: Table String
table = buildTable
 [ [ infx LeftAssociative "+"
   , infx LeftAssociative "-"
   ]
 ]

-- [(input, expected output)]
tests :: [(String,String)]
tests =
  [ ("1 + 2 - 3", "⟨- ⟨+ 1 2⟩ 3⟩")
  ]

