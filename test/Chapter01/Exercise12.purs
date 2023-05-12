module Test.Chapter01.Exercise12 where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.List as List
import Data.List.Types (List(..), (:))
import Data.List.Types as NEL
import Data.NonEmpty ((:|))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- Note: while this function is stack-safe, it is NOT heap-safe
inits :: forall a. List a -> NEL.NonEmptyList (List a)
inits = go Nil Nil
  where
  go :: List (List a) -> List a -> List a -> NEL.NonEmptyList (List a)
  go acc lastInit = case _ of
    Nil -> NEL.NonEmptyList $ Nil :| reverseInnards Nil acc
    Cons h t -> do
      let nextInit = Cons h lastInit
      go (Cons nextInit acc) nextInit t

  reverseInnards :: List (List a) -> List (List a) -> List (List a)
  reverseInnards acc = case _ of
    Nil -> acc
    Cons h t -> reverseInnards (Cons (List.reverse h) acc) t

-- Note: while this function is stack-safe, it is NOT heap-safe
tails :: forall a. List a -> NEL.NonEmptyList (List a)
tails = go Nil
  where
  go acc = case _ of
    Nil -> reverseAppend (pure Nil) acc
    ls@(Cons _ t) -> go (Cons ls acc) t

  reverseAppend initial = case _ of
    Nil -> initial
    Cons h t -> reverseAppend (NEL.nelCons h initial) t

spec :: Spec Unit
spec = describe "Exercise 12" do
  it "inits works on small inputs" do
    (NEA.fromFoldable1 $ inits (1 : 2 : 3 : Nil)) `shouldEqual`
      ( NonEmptyArray
          [ Nil
          , 1 : Nil
          , 1 : 2 : Nil
          , 1 : 2 : 3 : Nil
          ]
      )
  it "tails works on small inputs" do
    (NEA.fromFoldable1 $ tails (1 : 2 : 3 : Nil)) `shouldEqual`
      ( NonEmptyArray
          [ 1 : 2 : 3 : Nil
          , 2 : 3 : Nil
          , 3 : Nil
          , Nil
          ]
      )
