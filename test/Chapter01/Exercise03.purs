module Test.Chapter01.Exercise03 where

import Prelude

import Data.List (List(..))
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Exercise 3" do
  it "wrap/unwrap should roundtrip" do
    1 `shouldEqual` (unsafePartial $ unwrap (wrap 1))
  it "single Nil == false" do
    single (Nil :: List Int) `shouldEqual` false
  it "single Cons a Nil == true" do
    single (Cons 1 Nil) `shouldEqual` true
  it "single Cons a (Cons b Nil) == false" do
    single (Cons 1 (Cons 2 Nil)) `shouldEqual` false

wrap :: forall a. a -> List a
wrap a = Cons a Nil

unwrap :: forall a. Partial => List a -> a
unwrap = case _ of
  Cons a _ -> a

single :: forall a. List a -> Boolean
single = case _ of
  Cons _ Nil -> true
  _ -> false
