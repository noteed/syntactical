{-# Language TypeSynonymInstances #-}
-- Make String an (orphan) instance of Token. Give also a 'steps' function
-- to show the modified shunting-yard algorithm steps.
module Text.Syntactical.String where

import Data.List (intersperse)

import Text.Syntactical.Data (
  Token, operator,
  Tree(..), Table,
  partSymbol, previousPart
  )
import Text.Syntactical.Yard (Shunt(..), initial, isDone, step, Failure(..))

instance Token String where
  operator = concat

-- | Similar to the 'shunt' function but print the steps
-- performed by the modified shunting yard algorithm.
steps :: Table String -> [Tree String] -> IO ()
steps table ts = do
  putStrLn $ "               Input               Stack              Output   Rule"
  let sh = iterate (step table) $ initial ts
      l = length $ takeWhile (not . isDone) sh
  mapM_ (putStrLn . showShunt) (take (l + 1) sh)

-- | Give a textual representation of a 'Failure'.
showFailure :: Failure String -> String
showFailure f = case f of
  MissingBefore ps p ->
    "Parse error: missing operator parts " ++
    concatMap (\pt -> concat (intersperse " " pt)) ps ++ " before " ++ p
  MissingAfter p ps ->
    "Parse error: missing operator part " ++
    concat (intersperse ", " p) ++ " after " ++ concat (intersperse " " ps)
  CantMix a b ->
     "Parse error: cannot mix operators " ++ show a ++ " and " ++ show b
  MissingSubBetween a b ->
    "Parse error: no sub-expression between " ++ a ++ " and " ++ b
  MissingSubBefore a ->
    "Parse error: no sub-expression before " ++ a
  MissingSubAfter a ->
    "Parse error: no sub-expression after " ++ a
  Unexpected ->
    "Parsing raised a bug"

showShunt :: Shunt String -> String
showShunt (S ts ss os ru) =
  pad 20 ts ++ pad 20 ss ++ pads 20 os ++ "   " ++ show ru

showTree :: Tree String -> String
showTree = tail . f
  where
  f (Sym s) = ' ' : s
  f (Part y) = ' ' : concat (previousPart y ++ [partSymbol y])
  f (Node es) = ' ' : '⟨' : tail (concatMap f es) ++ "⟩"

pad :: Int -> [Tree String] -> String
pad n s =
  let s' = concat . (intersperse ", ") . map showTree $ s
  in replicate (n - length s') ' ' ++ s'

pads :: Int -> [[Tree String]] -> String
pads n s =
  let s' = concat . (intersperse ", ") .
        map (concat . (intersperse ", ") . map showTree) $ s
  in replicate (n - length s') ' ' ++ s'

