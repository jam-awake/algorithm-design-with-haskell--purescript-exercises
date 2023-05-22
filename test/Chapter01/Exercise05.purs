module Test.Chapter01.Exercise05 where

import Prelude

import Data.Foldable (for_)
import Data.List (List(..), foldl, foldr)
import Data.List as List
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- This is what the book says to do. It's stack-safe in PureScript
-- but only because the `List` type's `foldr` is implemented
-- as `foldl .... <<< List.reverse`.
bookMapFn :: forall a b. (a -> b) -> List a -> List b
bookMapFn f = foldr (\a tail -> Cons (f a) tail) Nil

-- This is a stack-safe implementation.
mapFn :: forall a b. (a -> b) -> List a -> List b
mapFn f = foldl (\tail a -> Cons (f a) tail) Nil <<< List.reverse

-- This is what the book says to do, but see comments in `bookMapFn`.
bookFilterFn :: forall a. (a -> Boolean) -> List a -> List a
bookFilterFn test = foldr (\a tail -> if test a then Cons a tail else tail) Nil

-- This is a stack-safe implementation.
filterFn :: forall a. (a -> Boolean) -> List a -> List a
filterFn test = foldl (\tail a -> if test a then Cons a tail else tail) Nil <<< List.reverse

spec :: Spec Unit
spec = describe "Exercise 5" do
  let
    allInputs =
      [ Tuple "empty input" $ (Nil :: List Int)
      , Tuple "small input" $ List.range 1 10
      , Tuple "medium input" $ List.range 1 100
      ]
  for_ allInputs \(Tuple msg input) -> do
    it ("bookMapFn == mapFn (" <> msg <> ")") do
      bookMapFn show input `shouldEqual` mapFn show input
      mapFn show input `shouldEqual` (map show input)
    it ("bookFilterFn == filterFn (" <> msg <> ")") do
      let isEven x = x `mod` 2 == 0
      bookFilterFn isEven input `shouldEqual` filterFn isEven input
      filterFn isEven input `shouldEqual` (List.filter isEven input)

