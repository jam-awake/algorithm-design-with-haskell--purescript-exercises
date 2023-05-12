module Test.Chapter01.Exercise21 where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

steep :: List Int -> Boolean
steep = go Nothing <<< List.reverse
  where
  go acc = case _ of
    Nil -> true
    Cons h t
      | Just acc' <- acc
      , h <= acc' -> false
      | otherwise -> go (Just $ maybe h (_ + h) acc) t

spec :: Spec Unit
spec = describe "Exercise 21" do
  let
    steepInputs = List.fromFoldable $ map (Tuple true <<< List.fromFoldable)
      [ [ 4, 2, 1, 0 ]
      , [ 1, 3, 2, -2, -3 ]
      , [ 0 ]
      ]
    notSteepInputs = List.fromFoldable $ map (Tuple false <<< List.fromFoldable)
      [ [ 2, 3, 0 ]
      , [ -4, -3, 0 ]
      ]

  for_ (steepInputs <> notSteepInputs) \(Tuple isSteep input) -> do
    it ("steep " <> show (Array.fromFoldable input) <> " == " <> show isSteep) do
      steep input `shouldEqual` isSteep
