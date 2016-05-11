module Enumerate.Main where
import Enumerate

main = do
  putStrLn "\nreifyCardinality @Bool..."
  print $ reifyCardinality [False]

  putStrLn "\n"
