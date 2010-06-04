module Main where

import Text.Syntactical.Indent (go', flatten)

echo :: String -> String
echo s = case go' s of
  Right a -> unwords $ flatten a ["\n"]
  Left err -> "error: " ++ show err ++ "\n"

main :: IO ()
main = interact echo

