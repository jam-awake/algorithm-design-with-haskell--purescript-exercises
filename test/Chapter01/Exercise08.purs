module Test.Chapter01.Exercise08 where

import Prelude

import Data.Foldable (foldr, for_)
import Data.List (List(..))
import Data.List as List
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

dropWhileEnd :: forall a. (a -> Boolean) -> List a -> List a
dropWhileEnd test = _.ls <<< flip foldr { drop: true, ls: Nil } \next acc ->
  if acc.drop && test next then acc
  else { drop: false, ls: Cons next acc.ls }

spec :: Spec Unit
spec = describe "Exercise 8" do
  let
    inputs =
      [ Tuple "empty" $ (Nil :: List Int)
      , Tuple "singleton" $ pure 1
      , Tuple "1..10" $ List.range 1 10
      ]
  for_ inputs \(Tuple size input) -> do
    describe ("For input with size " <> size) do
      it "dropWhileEnd == List.reverse <<< List.dropWhile <<< List.reverse" do
        let
          doTest p = shouldEqual
            (dropWhileEnd p input)
            (List.reverse $ List.dropWhile p $ List.reverse input)
        doTest \x -> x /= 0
        doTest \x -> x == 1
        doTest \x -> x == 10
        doTest \x -> x > 8
