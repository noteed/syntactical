{-# Language TypeSynonymInstances #-}
-- This module provides a 'steps' function to show the steps
-- of the modified sunting-yard algorithm on Strings.
module Text.Syntactical.String where

import Data.List (intersperse)

import Text.Syntactical.Data (
  SExpr(..), Tree(..), Table,
  partSymbol, previousPart,
  Token, operator
  )
import Text.Syntactical.Yard (Shunt(..), initial, isDone, step, Failure(..))

instance Token String where
  operator pt = Atom . concat $ previousPart pt ++ [partSymbol pt]

-- | Similar to the 'shunt' function but print the steps
-- performed by the modified shunting yard algorithm.
steps :: Table String -> [SExpr String] -> IO ()
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
  pad 20 ts ++ pad' 20 ss ++ pads 20 os ++ "   " ++ show ru

showTree :: Tree String -> String
showTree = tail . f
  where
  f (Leaf s) = ' ' : s
  f (Part y) = ' ' : concat (previousPart y ++ [partSymbol y])
  f (Branch []) = ' ' : "⟨⟩"
  f (Branch es) = ' ' : '⟨' : tail (concatMap f es) ++ "⟩"

showSExpr :: SExpr String -> String
showSExpr = tail . f
  where
  f (Atom s) = ' ' : s
  f (List []) = ' ' : "⟨⟩"
  f (List es) = ' ' : '⟨' : tail (concatMap f es) ++ "⟩"

bracket :: [String] -> String
bracket s = "[" ++ (concat . intersperse ",") s ++ "]"

pad' :: Int -> [Tree String] -> String
pad' n s =
  let s' = bracket . map showTree $ s
  in replicate (n - length s') ' ' ++ s'

pad :: Int -> [SExpr String] -> String
pad n s =
  let s' = bracket . map showSExpr $ s
  in replicate (n - length s') ' ' ++ s'

pads :: Int -> [[SExpr String]] -> String
pads n s =
  let s' = bracket .
        map (bracket . map showSExpr) $ s
  in replicate (n - length s') ' ' ++ s'

