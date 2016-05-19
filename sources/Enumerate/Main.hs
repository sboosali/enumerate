module Enumerate.Main where
import Enumerate
import Spiros.Prelude

main = do
  putStrLn "\nreifyCardinality @Bool..."
  print $ reifyCardinality [False]

  putStrLn "\n"
