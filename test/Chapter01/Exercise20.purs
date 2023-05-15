module Test.Chapter01.Exercise20 where

import Prelude

import Data.Foldable (foldl, for_)
import Data.List (List(..))
import Data.List as List
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

op :: forall a. (List a -> List a) -> List a -> List a -> List a
op f xs ys = f (xs <> ys)

spec :: Spec Unit
spec = describe "Exercise 20" do
  let
    concat :: forall a. List (List a) -> List a
    concat = join

    inputs =
      [ []
      , [ [ 1 ]
        ]
      , [ [ 1, 2, 3 ]
        ]
      , [ [ 1 ]
        , [ 2 ]
        , [ 3 ]
        ]
      , [ [ 1 ]
        , [ 2, 3 ]
        , [ 4, 5, 6 ]
        ]
      , [ [ 1 ]
        , []
        , [ 2, 3 ]
        , [ 4, 5, 6 ]
        , []
        ]
      ]
  describe "concat xss == foldl op identity xss Nil" do
    for_ inputs \i -> do
      it ("where `xss` = " <> show i) do
        let xss = List.fromFoldable $ map List.fromFoldable i
        (concat xss) `shouldEqual` (foldl op identity xss Nil)
