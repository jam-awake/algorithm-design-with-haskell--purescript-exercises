module Test.Chapter03.Exercise04 where

import Prelude

import Data.Foldable (for_)
import Data.FoldableWithIndex (foldlWithIndex, foldrWithIndex)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), isNothing)
import Data.Tuple (Tuple(..))
import Test.Chapter03.Code.SnocList (SnocList(..), (<:))
import Test.Chapter03.Code.SnocList as SnocList
import Test.Chapter03.Code.SymmetricList (SymmetricList(..))
import Test.Chapter03.Code.SymmetricList as SymmetricList
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

halveSLInit :: forall a. SnocList a -> Maybe { head :: a, init :: List a, tail :: SnocList a }
halveSLInit ls = splitSLInit (SnocList.length ls / 2) ls

splitSLInit :: forall a. Int -> SnocList a -> Maybe { head :: a, init :: List a, tail :: SnocList a }
splitSLInit midPoint ls = do
  let
    value = ls # flip foldrWithIndex Nothing \idx next acc0 -> do
      if isNothing acc0 then
        Just $ { head: next, init: Nil, tail: Nil }
      else if idx <= midPoint then
        acc0 <#> \acc ->
          acc { head = next, tail = acc.head : acc.tail }
      else
        acc0 <#> \acc ->
          acc { head = next, init = acc.head : acc.init }

  value <#> \value' -> value' { tail = SnocList.fromList value'.tail }

unconsSL :: forall a. SymmetricList a -> Maybe { head :: a, tail :: SymmetricList a }
unconsSL = case _ of
  Empty ->
    Nothing
  Single a ->
    Just { head: a, tail: Empty }
  Ends head init tail last ->
    Just $ { head, tail: _ } $ case init of
      Cons a init' ->
        Ends a init' tail last
      Nil -> case halveSLInit tail of
        Nothing -> Single last
        Just r -> Ends r.head r.init r.tail last

tailSL :: forall a. SymmetricList a -> Maybe (SymmetricList a)
tailSL = case _ of
  Empty -> Nothing
  Single _ -> Just Empty
  Ends _ init tail last -> Just case List.uncons init of
    Just initR -> Ends initR.head initR.tail tail last
    Nothing -> case halveSLInit tail of
      Just tailR -> Ends tailR.head tailR.init tailR.tail last
      Nothing -> Single last

halveListLast :: forall a. List a -> Maybe { init :: List a, tail :: SnocList a, last :: a }
halveListLast ls = splitListLast (List.length ls / 2) ls

splitListLast :: forall a. Int -> List a -> Maybe { init :: List a, tail :: SnocList a, last :: a }
splitListLast midPoint ls = do
  let
    value = ls # flip foldlWithIndex Nothing \idx acc0 next -> do
      if isNothing acc0 then
        Just { init: SnocNil, tail: SnocNil, last: next }
      else if idx <= midPoint then
        acc0 <#> \acc ->
          acc { init = acc.init <: acc.last, last = next }
      else
        acc0 <#> \acc ->
          acc { tail = acc.tail <: acc.last, last = next }

  value <#> \value' -> value' { init = SnocList.toList value'.init }

unsnocSL :: forall a. SymmetricList a -> Maybe { init :: SymmetricList a, last :: a }
unsnocSL = case _ of
  Empty ->
    Nothing
  Single a ->
    Just { init: Empty, last: a }
  Ends head init tail last -> Just $ { init: _, last } $ case tail of
    Snoc tail' last' -> Ends head init tail' last'
    SnocNil -> case halveListLast init of
      Nothing -> Single head
      Just initR -> Ends head initR.init initR.tail initR.last

initSL :: forall a. SymmetricList a -> Maybe (SymmetricList a)
initSL = case _ of
  Empty ->
    Nothing
  Single _ -> Just Empty
  Ends head init tail _ -> Just case tail of
    Snoc tail' last' -> Ends head init tail' last'
    SnocNil -> case halveListLast init of
      Nothing -> Single head
      Just initR -> Ends head initR.init initR.tail initR.last

spec :: Spec Unit
spec = describe "Exercise 4" do
  describe "halveSLInit" do
    it "empty" do
      halveSLInit (SnocNil :: SnocList Int) `shouldEqual` (Nothing :: Maybe { head :: Int, init :: List Int, tail :: SnocList Int })
    it "singleton" do
      halveSLInit (SnocNil <: 1) `shouldEqual` (Just { head: 1, init: Nil, tail: SnocNil })
    it "two elems" do
      halveSLInit (SnocNil <: 1 <: 2) `shouldEqual` (Just { head: 1, init: Nil, tail: SnocNil <: 2 })
    it "three elems" do
      halveSLInit (SnocNil <: 1 <: 2 <: 3) `shouldEqual` (Just { head: 1, init: Cons 2 Nil, tail: SnocNil <: 3 })

  let
    inputs =
      [ Tuple "Empty list" Nil
      , Tuple "Singleton list" $ pure 1
      , Tuple "list of 1..10" $ List.range 1 10
      ]
  for_ inputs \(Tuple msg i) -> do
    let symList = SymmetricList.fromList i
    describe msg do
      it "unconsSL should work" do
        let convertTail { head, tail } = { head, tail: SymmetricList.toList tail }
        (map convertTail $ unconsSL symList) `shouldEqual` List.uncons i
      it "tailSL should work" do
        (map SymmetricList.toList $ tailSL symList) `shouldEqual` List.tail i
      it "unsnocSL should work" do
        let convertInit { init, last } = { init: SymmetricList.toList init, last }
        (map convertInit $ unsnocSL symList) `shouldEqual` List.unsnoc i
      it "initSL should work" do
        (map SymmetricList.toList $ initSL symList) `shouldEqual` List.init i
