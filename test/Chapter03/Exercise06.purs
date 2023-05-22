module Test.Chapter03.Exercise06 where

import Prelude

import Data.List (List(..), (:))
import Test.Chapter01.Exercise12 (NonEmptyR, (|:))
import Test.Chapter01.Exercise12.SnocList (SnocList(..), (<:))
import Test.Chapter01.Exercise12.SnocList as SnocList
import Test.Chapter03.Code.SymmetricList (SymmetricList(..))
import Test.Chapter03.Code.SymmetricList as SymmetricList
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

snocSL :: forall a. SymmetricList a -> a -> SymmetricList a
snocSL ls a = case ls of
  Empty -> Single a
  Single b -> Ends b Nil SnocNil a
  Ends head init tail last -> Ends head init (tail <: last) a

-- This is a stack-safe and heap-safe implementation.
-- It's heap-safe because it always `Snoc`s data
-- onto the `tail` part of an `End`, thereby sharing data via the `SnocList`.
initsSL :: forall a. SymmetricList a -> NonEmptyR SymmetricList (SymmetricList a)
initsSL = case _ of
  ls@Empty -> Empty |: ls
  ls@(Single _) -> Single Empty |: ls
  ls@(Ends h i t _) -> do
    let
      initRuns1 |: prevRun1 = goInits (Single Empty) (Single h) i
      -- Since we're going to iterate through the entire `tail` part,
      -- there's no reason to split the `tail` part multiple times
      -- as we would if we used `initSL` repeatedly.
      initRuns2 |: prevRun2 = goInits initRuns1 prevRun1 $ SnocList.toList t
    (snocSL initRuns2 prevRun2) |: ls
  where
  goInits :: SymmetricList (SymmetricList a) -> SymmetricList a -> List a -> NonEmptyR SymmetricList (SymmetricList a)
  goInits initRuns prevRun = case _ of
    Nil ->
      initRuns |: prevRun
    Cons h t -> do
      goInits (snocSL initRuns prevRun) (snocSL prevRun h) t

spec :: Spec Unit
spec = describe "Exercise 6" do
  describe "initsSL" do
    it "works on empty input" do
      initsSL (SymmetricList.fromList Nil) `shouldEqual`
        (Empty |: (Empty :: SymmetricList Int))
    it "works on single input" do
      initsSL (SymmetricList.fromList $ pure 1) `shouldEqual`
        (Single Empty |: Single 1)
    it "works on two-value input" do
      initsSL (SymmetricList.fromList $ 1 : 2 : Nil) `shouldEqual`
        ( ( Ends
              (SymmetricList.fromList Nil)
              Nil
              SnocNil
              (SymmetricList.fromList $ 1 : Nil)
          )
            |: (SymmetricList.fromList $ 1 : 2 : Nil)
        )
    it "works on multi-value input" do
      initsSL (SymmetricList.fromList $ 1 : 2 : 3 : 4 : Nil) `shouldEqual`
        ( ( Ends
              (SymmetricList.fromList Nil)
              Nil
              (SnocNil <: (SymmetricList.fromList $ 1 : Nil) <: (SymmetricList.fromList $ 1 : 2 : Nil))
              (SymmetricList.fromList $ 1 : 2 : 3 : Nil)
          )
            |: (SymmetricList.fromList $ 1 : 2 : 3 : 4 : Nil)
        )

