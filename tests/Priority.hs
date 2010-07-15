module Priority where

import Text.Syntactical
import Text.Syntactical.Data

table :: Table String
table = buildTable
 [ [ infx LeftAssociative "+"
   , infx LeftAssociative "-"
   ]
 , [ infx LeftAssociative "al7"
   , infx RightAssociative "ar7"
   ]
 , [ infx LeftAssociative "al6"
   , infx RightAssociative "ar6"
   , infx NonAssociative "a6"
   , infx LeftAssociative "bl6"
   , infx RightAssociative "br6"
   , infx NonAssociative "b6"
   ]
 , [ infx LeftAssociative "al5"
   , infx NonAssociative "a5"
   , infx NonAssociative "b5"
   ]
 ]

-- [(input, expected output)]
tests :: [(String,String)]
tests =
  [ ("1 + 2 - 3", "⟨- ⟨+ 1 2⟩ 3⟩")
  , ("1 - 2 + 3", "⟨+ ⟨- 1 2⟩ 3⟩")
  , ("1 + 2 - 3 + 4", "⟨+ ⟨- ⟨+ 1 2⟩ 3⟩ 4⟩")

  , ("X al7 Y bl6 Z", "⟨bl6 ⟨al7 X Y⟩ Z⟩")
  , ("X ar7 Y br6 Z", "⟨br6 ⟨ar7 X Y⟩ Z⟩")
  , ("X al5 Y bl6 Z", "⟨al5 X ⟨bl6 Y Z⟩⟩")
  , ("X al5 Y br6 Z", "⟨al5 X ⟨br6 Y Z⟩⟩")
  , ("X a5  Y b6  Z", "⟨a5 X ⟨b6 Y Z⟩⟩")
  , ("X a5  Y br6 Z", "⟨a5 X ⟨br6 Y Z⟩⟩")
  , ("X al5 Y b6  Z", "⟨al5 X ⟨b6 Y Z⟩⟩")

  , ("X al6 Y bl6 Z", "⟨bl6 ⟨al6 X Y⟩ Z⟩")

  , ("X ar6 Y br6 Z", "⟨ar6 X ⟨br6 Y Z⟩⟩")

-- ("X a5 Y b5 Z", parse error)
  ]

