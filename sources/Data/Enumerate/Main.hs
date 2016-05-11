module Data.Enumerate.Main where
import Data.Enumerate

main = do
  putStrLn "\nreifyCardinality @Bool..."
  print $ reifyCardinality [False]

  putStrLn "\n"
