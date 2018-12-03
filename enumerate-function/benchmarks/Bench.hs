import Enumerate()
import Criterion.Main
import Prelude

main = defaultMain [
  bgroup "Enumerate"
    [ bench "1" $ nf   length [1 .. 1000::Int]
    , bench "2" $ whnf length [1 .. 1000::Int]
    ]
  ]

