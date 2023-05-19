module Test.Chapter03.Exercise03 where

import Prelude

import Data.Foldable (for_)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Test.Chapter01.Exercise12.SnocList (SnocList(..))
import Test.Chapter03.Code.SymmetricList (SymmetricList(..))
import Test.Chapter03.Code.SymmetricList as SymmetricList
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

consSL :: forall a. a -> SymmetricList a -> SymmetricList a
consSL a = case _ of
  Empty -> Single a
  Single b -> Ends a Nil SnocNil b
  Ends head init tail last -> Ends a (head : init) tail last

headSL :: forall a. SymmetricList a -> Maybe a
headSL = case _ of
  Empty -> Nothing
  Single a -> Just a
  Ends first _ _ _ -> Just first

spec :: Spec Unit
spec = describe "Exercise 3" do
  let
    inputs =
      [ Tuple "Empty list" Nil
      , Tuple "Singleton list" $ pure 1
      , Tuple "list of 1..10" $ List.range 1 10
      ]
  for_ inputs \(Tuple msg i) -> do
    describe msg do
      it "consSL should work" do
        let expected = Cons 99 i
        let actual = consSL 99 $ SymmetricList.fromList i
        SymmetricList.toList actual `shouldEqual` expected
      it "headSL should work" do
        List.head i `shouldEqual` (headSL $ SymmetricList.fromList i)
