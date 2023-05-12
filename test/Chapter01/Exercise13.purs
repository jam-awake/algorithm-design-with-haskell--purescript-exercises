module Test.Chapter01.Exercise13 where

import Prelude

import Data.Foldable (for_)
import Data.List as List
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

applyNTimes :: forall a. Int -> (a -> a) -> a -> a
applyNTimes remaining f start
  | remaining <= 0 = start
  | otherwise = applyNTimes (remaining - 1) f (f start)

spec :: Spec Unit
spec = describe "Exercise 13" do
  for_ (List.range (-3) 10) \i ->
    it ("applyNTimes " <> show i <> " (_ + 1) 0 == " <> show (max 0 i)) do
      applyNTimes i (_ + 1) 0 `shouldEqual` (max 0 i)
