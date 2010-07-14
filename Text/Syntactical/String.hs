{-# Language TypeSynonymInstances #-}
-- This module provides a 'steps' function to show the steps
-- of the modified sunting-yard algorithm on Strings.
module Text.Syntactical.String where

import Data.List (intersperse)

import Text.Syntactical.Data (
  SExpr(..), Tree(..), Part(..), Table,
  partSymbol, previousPart,
  Token, toString, operator
  )
import Text.Syntactical.Yard (
  Shunt(..), initial, isDone, step, Failure(..), Rule(..)
  )

instance Token String where
  toString = id
  operator pt as = List $
    (Atom . concat $ previousPart pt ++ [partSymbol pt]) : as

-- | Similar to the 'shunt' function but print the steps
-- performed by the modified shunting yard algorithm.
steps :: Token a => Table a -> [SExpr a] -> IO ()
steps table ts = do
  putStrLn $ "               Input               Stack              Output   Rule"
  let sh = iterate (step table) $ initial ts
      l = length $ takeWhile (not . isDone) sh
  mapM_ (putStrLn . showShunt) (take (l + 1) sh)

-- | Give a textual representation of a 'Failure'.
showFailure :: Token a => Failure a -> String
showFailure f = case f of
  MissingBefore ps p ->
    "Parse error: missing operator parts " ++
    concatMap (\pt -> concat (intersperse " " $ map toString pt)) ps ++
    " before " ++ toString p
  MissingAfter p ps ->
    "Parse error: missing operator part " ++
    concat (intersperse ", " $ map toString p) ++ " after " ++
    concat (intersperse " " $ map toString ps)
  CantMix a b ->
     "Parse error: cannot mix operators " ++ showPart a ++
     " and " ++ showPart b
  MissingSubBetween a b ->
    "Parse error: no sub-expression between " ++ toString a ++
    " and " ++ toString b
  MissingSubBefore a ->
    "Parse error: no sub-expression before " ++ toString a
  MissingSubAfter a ->
    "Parse error: no sub-expression after " ++ toString a
  Unexpected ->
    "Parsing raised a bug"

-- TODO but the CantMix failure case is unused
showPart :: Token a => Part a -> String
showPart = undefined

--TODO
showRule :: Token a => Rule a -> String
showRule = undefined

showShunt :: Token a => Shunt a -> String
showShunt (S ts ss os ru) =
  pad 20 ts ++ pad' 20 ss ++ pads 20 os ++ "   " ++ showRule ru

showTree :: Token a => Tree a -> String
showTree = tail . f
  where
  f (Leaf s) = ' ' : toString s
  f (Part y) = ' ' : concatMap toString (previousPart y ++ [partSymbol y])
  f (Branch []) = ' ' : "⟨⟩"
  f (Branch es) = ' ' : '⟨' : tail (concatMap f es) ++ "⟩"

showSExpr :: Token a => SExpr a -> String
showSExpr = tail . f
  where
  f (Atom s) = ' ' : toString s
  f (List []) = ' ' : "⟨⟩"
  f (List es) = ' ' : '⟨' : tail (concatMap f es) ++ "⟩"

bracket :: [String] -> String
bracket s = "[" ++ (concat . intersperse ",") s ++ "]"

pad' :: Token a => Int -> [Tree a] -> String
pad' n s =
  let s' = bracket . map showTree $ s
  in replicate (n - length s') ' ' ++ s'

pad :: Token a => Int -> [SExpr a] -> String
pad n s =
  let s' = bracket . map showSExpr $ s
  in replicate (n - length s') ' ' ++ s'

pads :: Token a => Int -> [[SExpr a]] -> String
pads n s =
  let s' = bracket .
        map (bracket . map showSExpr) $ s
  in replicate (n - length s') ' ' ++ s'

