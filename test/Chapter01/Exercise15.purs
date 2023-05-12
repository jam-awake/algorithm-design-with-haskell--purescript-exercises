module Test.Chapter01.Exercise15 where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Data.List (List(..), foldl, (:))
import Data.List as List
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

remove :: forall a. Eq a => a -> List a -> List a
remove a origList = go Nil origList
  where
  go acc = case _ of
    Cons h t
      | a == h -> foldl (flip Cons) t acc
      | otherwise -> go (Cons h acc) t
    Nil -> origList

spec :: Spec Unit
spec = describe "Exercise 15" do
  let
    initialList = 1 : 2 : 3 : 4 : 5 : Nil
    specialElem = 12
    inputs = do
      amt <- Array.range 1 $ List.length initialList
      duplicateElem <- [ true, false ]
      if duplicateElem then do
        let
          before = List.take amt initialList
          after = List.drop amt initialList
        pure
          { input: before <> (specialElem : specialElem : after)
          , expected: before <> (specialElem : after)
          }
      else
        pure
          { input: List.take amt initialList <> (Cons specialElem $ List.drop amt initialList)
          , expected: initialList
          }
  for_ inputs \r -> do
    it ("remove " <> show specialElem <> " " <> (show $ Array.fromFoldable r.input) <> " == " <> (show $ Array.fromFoldable r.expected)) do
      remove specialElem r.input `shouldEqual` r.expected
  it "remove 0 [] == []" do
    remove 0 (Nil :: List Int) `shouldEqual` (Nil :: List Int)
