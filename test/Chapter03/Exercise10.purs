module Test.Chapter03.Exercise10 where

import Prelude

import Data.Foldable (foldr, for_)
import Data.List (List(..))
import Data.List as List
import Data.Tuple (Tuple(..))
import Test.Chapter03.Code.RandomAccessList (RandomAccessList, consRA)
import Test.Chapter03.Code.RandomAccessList as RandomAccessList
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

toRandomAccessList :: forall a. List a -> RandomAccessList a
toRandomAccessList = foldr consRA Nil

spec :: Spec Unit
spec = describe "Exercise 10" do
  let
    inputs =
      [ Tuple "empty list" (Nil :: List Int)
      , Tuple "singleton list" $ pure 1
      , Tuple "1..10" $ List.range 1 10
      ]
  for_ inputs \(Tuple msg i) -> do
    it (msg <> " - toRandomAccessList produces the same list") do
      (RandomAccessList.toList $ toRandomAccessList i) `shouldEqual` i
