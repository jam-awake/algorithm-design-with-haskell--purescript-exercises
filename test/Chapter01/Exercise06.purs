module Test.Chapter01.Exercise06 where

import Prelude

import Data.Foldable (for_)
import Data.List (List(..), foldr)
import Data.List as List
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

original :: forall a b. (a -> b -> b) -> b -> (a -> Boolean) -> List a -> b
original f e p = foldr f e <<< List.filter p

foldrFilter :: forall a b. (a -> b -> b) -> b -> (a -> Boolean) -> List a -> b
foldrFilter f e p = foldr (\a b -> if p a then f a b else b) e

spec :: Spec Unit
spec = describe "Exercise 6" do
  let
    inputs =
      [ Tuple "1..10" $ List.range 1 10
      , Tuple "empty" Nil
      ]
  for_ inputs \(Tuple msg input) -> do
    it ("original == foldrFilter (" <> msg <> ")") do
      let doMainTest f e p = (original f e p input) `shouldEqual` (foldrFilter f e p input)
      let isEven x = x `mod` 2 == 0
      doMainTest add 0 isEven
      (foldrFilter add 0 isEven $ List.range 2 3) `shouldEqual` 2
