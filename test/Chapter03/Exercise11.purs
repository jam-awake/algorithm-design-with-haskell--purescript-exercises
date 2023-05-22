module Test.Chapter03.Exercise11 where

import Prelude

import Data.Foldable (foldr, for_)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import Test.Chapter01.Exercise12.SnocList (SnocList(..), (<:))
import Test.Chapter03.Code.RandomAccessList (Digit(..), Tree(..), RandomAccessList, treeSize)
import Test.Chapter03.Code.RandomAccessList as RandomAccessList
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- Again, the book does not provide a stack-safe implementation.
-- So we define one here.
updateRA :: forall a. Int -> a -> RandomAccessList a -> RandomAccessList a
updateRA = updateRA' SnocNil
  where
  updateRA' acc idx a ls = case ls of
    Nil -> foldr Cons Nil acc
    Cons h tail -> case h of
      Zero -> updateRA' (acc <: h) idx a tail
      One t -> do
        let size = treeSize t
        if idx < size then do
          let
            newTree = updateTree SnocNil idx a t
            newDigit = One newTree : tail
          foldr Cons newDigit acc
        else
          updateRA' (acc <: h) (idx - size) a tail

updateTree :: forall a. (SnocList (Tree a -> Tree a)) -> Int -> a -> Tree a -> Tree a
updateTree treeAcc idx a t = case t of
  Leaf _
    | idx == 0 -> foldr (\fn acc -> fn acc) (Leaf a) treeAcc
    | otherwise -> t -- outside the bounds of the list
  Node size l r -> do
    let m = size / 2
    if idx < m then updateTree (treeAcc <: (\l' -> Node size l' r)) idx a l
    else updateTree (treeAcc <: (\r' -> Node size l r')) (idx - m) a r

spec :: Spec Unit
spec = describe "Exercise 11" do
  let
    newValue = 99
    inputs =
      [ { msg: "empty list", input: Nil, index: 0, output: Nil }
      , { msg: "singleton list (within bounds)", input: pure 1, index: 0, output: pure newValue }
      , { msg: "singleton list (out of bounds)", input: pure 1, index: 9, output: pure 1 }
      , { msg: "1..10 (within bounds)", input: List.range 1 10, index: 4, output: unsafePartial $ fromJust $ List.modifyAt 4 (const newValue) $ List.range 1 10 }
      , { msg: "1..10 (check off-by-one)", input: List.range 1 10, index: 9, output: unsafePartial $ fromJust $ List.modifyAt 9 (const newValue) $ List.range 1 10 }
      , { msg: "1..10 (out of bounds)", input: List.range 1 10, index: 20, output: List.range 1 10 }
      ]
  describe "updateRA" do
    for_ inputs \r -> do
      it r.msg do
        (RandomAccessList.toList $ updateRA r.index newValue $ RandomAccessList.fromList r.input) `shouldEqual` r.output

