module Test.Chapter01.Exercise04 where

import Prelude

import Data.List (List(..), foldl, (:))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Exercise 4" do
  it "3 : 2 : 1 : Nil == reverse $ 1 : 2 : 3 : Nil" do
    (3 : 2 : 1 : Nil) `shouldEqual` (reverse $ 1 : 2 : 3 : Nil)

reverse :: forall a. List a -> List a
reverse = foldl (flip Cons) Nil
