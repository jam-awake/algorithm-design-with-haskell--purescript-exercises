module Test.Chapter01.Exercise02 where

import Prelude

import Data.List.Types (List(..))
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Exercise 2" do
  it "uncons Nil == Nothing" do
    uncons (Nil :: List Int) `shouldEqual` Nothing
  it "uncons Cons 1 Nil == Just" do
    uncons (Cons 1 Nil) `shouldEqual` (Just { head: 1, tail: Nil })

uncons :: forall a. List a -> Maybe { head :: a, tail :: List a }
uncons = case _ of
  Nil -> Nothing
  Cons head tail -> Just { head, tail }
