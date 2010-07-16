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
 , [ al6
   , ar6
   , a6
   , bl6
   , br6
   , b6
   ]
 , [ infx LeftAssociative "al5"
   , a5
   , b5
   ]
 ]

a5 = 1 `setPrecedence` infx NonAssociative "a5"
b5 = 1 `setPrecedence` infx NonAssociative "b5"
a6 = 2 `setPrecedence` infx NonAssociative "a6"
al6 = 2 `setPrecedence` infx LeftAssociative "al6"
ar6 = 2 `setPrecedence` infx RightAssociative "ar6"
bl6 = 2 `setPrecedence` infx LeftAssociative "bl6"
br6 = 2 `setPrecedence` infx RightAssociative "br6"
b6 = 2 `setPrecedence` infx NonAssociative "b6"

a5' = head $ cut a5
b5' = head $ cut b5
a6' = head $ cut a6
al6' = head $ cut al6
ar6' = head $ cut ar6
bl6' = head $ cut bl6
br6' = head $ cut br6
b6' = head $ cut b6

-- [(input, expected output)]
tests :: [(String,String)]
tests =
  [ ("1 + 2 - 3", "⟨␣-␣ ⟨␣+␣ 1 2⟩ 3⟩")
  , ("1 - 2 + 3", "⟨␣+␣ ⟨␣-␣ 1 2⟩ 3⟩")
  , ("1 + 2 - 3 + 4", "⟨␣+␣ ⟨␣-␣ ⟨␣+␣ 1 2⟩ 3⟩ 4⟩")

  , ("X al7 Y bl6 Z", "⟨␣bl6␣ ⟨␣al7␣ X Y⟩ Z⟩")
  , ("X ar7 Y br6 Z", "⟨␣br6␣ ⟨␣ar7␣ X Y⟩ Z⟩")
  , ("X al5 Y bl6 Z", "⟨␣al5␣ X ⟨␣bl6␣ Y Z⟩⟩")
  , ("X al5 Y br6 Z", "⟨␣al5␣ X ⟨␣br6␣ Y Z⟩⟩")
  , ("X a5  Y b6  Z", "⟨␣a5␣ X ⟨␣b6␣ Y Z⟩⟩")
  , ("X a5  Y br6 Z", "⟨␣a5␣ X ⟨␣br6␣ Y Z⟩⟩")
  , ("X al5 Y b6  Z", "⟨␣al5␣ X ⟨␣b6␣ Y Z⟩⟩")

  , ("X al6 Y bl6 Z", "⟨␣bl6␣ ⟨␣al6␣ X Y⟩ Z⟩")

  , ("X ar6 Y br6 Z", "⟨␣ar6␣ X ⟨␣br6␣ Y Z⟩⟩")

  , ("X al6 Y al5 Z al7 X", "⟨␣al5␣ ⟨␣al6␣ X Y⟩ ⟨␣al7␣ Z X⟩⟩")
  ]

tests' :: [(String,Failure String)]
tests' =
  [ ("X a5 Y b5 Z", CantMix b5' a5')
  , ("X a6 Y bl6 Z", CantMix bl6' a6')
  , ("X al6 Y b6 Z", CantMix b6' al6')
  , ("X ar6 Y b6 Z", CantMix b6' ar6')
  , ("X a6 Y br6 Z", CantMix br6' a6')
  , ("X al6 Y br6 Z", CantMix br6' al6')
  , ("X ar6 Y bl6 Z", CantMix bl6' ar6')
  ]

