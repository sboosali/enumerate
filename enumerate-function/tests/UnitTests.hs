-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import "tasty"            Test.Tasty
import "tasty-hunit"      Test.Tasty.HUnit
import "tasty-quickcheck" Test.Tasty.QuickCheck

--------------------------------------------------

import "base" Data.List
import "base" Data.Ord

--------------------------------------------------

import "base" Prelude

--------------------------------------------------
-- Main ------------------------------------------
--------------------------------------------------

main :: IO ()
main = defaultMain $

  testGroup "Tests" allTests

--------------------------------------------------

allTests :: [TestTree]
allTests =

    [ testGroup "Unit Tests" unitTests

    , testGroup "Property Tests"

        [ testGroup "(via SmallCheck)" []
        , testGroup "(via QuickCheck)" propertyTests
        ]
    ]

--------------------------------------------------
-- Tests -----------------------------------------
--------------------------------------------------

unitTests :: [TestTree]
unitTests = 
        [ testCase "List comparison (different length)" $

            ([1, 2, 3] `compare` [1, (2::Integer)]) @?= GT

        , testCase "List comparison (same length)" $

            ([1, 2, 3] `compare` [1, 2, (2::Integer)]) @?= GT
        ]

--------------------------------------------------

propertyTests :: [TestTree]
propertyTests =
        
          [ testProperty "sort == sort . reverse" $

              \list ->
                sort (list :: [Int]) == sort (reverse list)

          -- , testProperty "Fermat's last theorem" $

          --     \(a :: Int) (b :: Int) (c :: Int) (n :: Int) ->
          --       (n >= 3) ==>
          --         (a >= 1) && (b >= 1) && (c >= 1) ==>
          --           a^n + b^n /= c^n
          ]

-- "In number theory, Fermat's Last Theorem states that no three positive integers a, b, and c satisfy the equation aⁿ + bⁿ = cⁿ for any integer value of n greater than 2. The cases n = 1 and n = 2 have been known to have infinitely many solutions since antiquity." — « https://en.m.wikipedia.org/wiki/Fermat's_Last_Theorem »

--------------------------------------------------
--------------------------------------------------