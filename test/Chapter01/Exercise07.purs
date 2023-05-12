module Test.Chapter01.Exercise07 where

import Prelude

import Data.Foldable (foldr, for_)
import Data.Lazy as Lazy
import Data.List as List
import Data.List.Lazy.Types as LL
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- This is how we'd write things in Haskell.
-- In Haskell, it doesn't process the entirety of the list due to laziness
-- In PureScript, a strict runtime, this will process through the entire list.
-- As such, it's not stack-safe.
takeWhileLazyInvisible :: forall a. (a -> Boolean) -> List.List a -> List.List a
takeWhileLazyInvisible p = foldr (\a tail -> if p a then List.Cons a tail else tail) List.Nil

takeWhileLazyVisible :: forall a. (a -> Boolean) -> LL.List a -> LL.List a
takeWhileLazyVisible p = go <<< Lazy.force <<< unwrap
  where
  go :: LL.Step a -> LL.List a
  go = case _ of
    LL.Cons h tail
      | p h ->
          LL.List $ Lazy.defer \_ ->
            LL.Cons h $ go $ Lazy.force $ unwrap tail
      | otherwise ->
          LL.List $ Lazy.defer \_ ->
            LL.Nil
    LL.Nil ->
      LL.List $ Lazy.defer \_ ->
        LL.Nil

takeWhileStrict :: forall a. (a -> Boolean) -> List.List a -> List.List a
takeWhileStrict p = go List.Nil
  where
  go acc = case _ of
    List.Cons h tail | p h -> go (List.Cons h acc) tail
    -- These last two cases could be covered via a wildcard
    -- _ -> List.reverse acc
    List.Cons _ _ -> List.reverse acc
    List.Nil -> List.reverse acc

spec :: Spec Unit
spec = describe "Exercise 7" do
  let
    inputs =
      [ Tuple "empty" (List.Nil :: List.List Int)
      , Tuple "singleton" $ pure 1
      , Tuple "1..10" $ List.range 1 10
      ]
  for_ inputs \(Tuple msg input) -> do
    let isLessThan4 x = x < 4
    let inputLazyList = foldr LL.cons LL.nil input
    describe ("Using input with size " <> msg) do
      it "takeWhileLazyInvisible == takeWhileLazyVisible" do
        let
          doTest p = shouldEqual
            (takeWhileLazyInvisible p input)
            (List.fromFoldable $ takeWhileLazyVisible p inputLazyList)
        doTest isLessThan4
      it "takeWhileLazyInvisible == takeWhileStrict" do
        let
          doTest p = shouldEqual
            (takeWhileLazyInvisible p input)
            (takeWhileStrict p input)
        doTest isLessThan4
      it "takeWhileLazyVisible == takeWhileStrict" do
        let
          doTest p = shouldEqual
            (List.fromFoldable $ takeWhileLazyVisible p inputLazyList)
            (takeWhileStrict p input)
        doTest isLessThan4
