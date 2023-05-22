module Test.Chapter03.Exercise13 where

import Prelude

import Data.Foldable (for_)
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Test.Chapter03.Code.RandomAccessList (RandomAccessList, consRA, unconsRA)
import Test.Chapter03.Code.RandomAccessList as RandomAccessList
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

headRA :: forall a. RandomAccessList a -> Maybe a
headRA = map _.head <<< unconsRA

tailRA :: forall a. RandomAccessList a -> Maybe (RandomAccessList a)
tailRA = map _.tail <<< unconsRA

spec :: Spec Unit
spec = describe "Exercise 13" do
  let
    inputs =
      [ Tuple "empty input" $ (Nil :: List Int)
      , Tuple "singleton list" $ pure 1
      , Tuple "1..10" $ List.range 1 10
      ]
  describe "headRA" do
    for_ inputs \(Tuple msg i) -> do
      it msg do
        let h = 99
        (headRA $ consRA h $ RandomAccessList.fromList i) `shouldEqual` Just h
  describe "tailRA" do
    for_ inputs \(Tuple msg i) -> do
      it msg do
        let h = 99
        (map RandomAccessList.toList $ tailRA $ consRA h $ RandomAccessList.fromList i) `shouldEqual` Just i
