module Test.Chapter03.Exercise02 where

import Prelude

import Data.List (List(..), (:))
import Data.List as List
import Test.Chapter03.Code.SymmetricList (SymmetricList(..))
import Test.Chapter03.Code.SnocList (SnocList(..), (<:))
import Test.Chapter03.Code.SnocList as SnocList
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

nilSL :: forall a. SymmetricList a
nilSL = Empty

nullSL :: forall a. SymmetricList a -> Boolean
nullSL = case _ of
  Empty -> true
  _ -> false

singleSL :: forall a. SymmetricList a -> Boolean
singleSL = case _ of
  Single _ -> true
  _ -> false

lengthSL :: forall a. SymmetricList a -> Int
lengthSL = case _ of
  Ends _ init tail _ -> 2 + List.length init + SnocList.length tail
  Single _ -> 1
  Empty -> 0

spec :: Spec Unit
spec = describe "Exercise 2" do
  it "nullSL should work" do
    nullSL Empty `shouldEqual` true
    nullSL (Single 1) `shouldEqual` false
    nullSL (Ends 1 Nil SnocNil 2) `shouldEqual` false
  it "lengthSL should work" do
    lengthSL Empty `shouldEqual` 0
    lengthSL (Single 1) `shouldEqual` 1
    lengthSL (Ends 1 Nil SnocNil 2) `shouldEqual` 2
    lengthSL (Ends 1 (2 : Nil) (SnocNil <: 3 <: 4) 5) `shouldEqual` 5
