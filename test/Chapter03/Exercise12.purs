module Test.Chapter03.Exercise12 where

import Prelude

import Data.Foldable (foldl)
import Data.List (List)
import Data.List as List
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Test.Chapter03.Code.RandomAccessList (RandomAccessList)
import Test.Chapter03.Code.RandomAccessList as RandomAccessList
import Test.Chapter03.Exercise11 (updateRA)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

updates :: forall a. RandomAccessList a -> List (Tuple Int a) -> RandomAccessList a
updates ls lToRMods = {-
  foldl (flip <<< uncurry updatRA) ls lToRMods                  -} 
  foldl (\ls' (Tuple i a) -> updateRA i a ls') ls lToRMods

spec :: Spec Unit
spec = describe "Exercise 12" do
  it "updates works from left-to-right" do
    let
      initialList = List.range 1 10
      raList = RandomAccessList.fromList initialList
      updateList = List.fromFoldable
        [ Tuple 0 99 -- first update is overwritten by next one
        , Tuple 0 98
        , Tuple 1 40 -- update unrelated to first group
        , Tuple 20 53 -- this update doesn't happen due to being out-of-bounds
        ]
      expectedList = updateList # flip foldl initialList \acc (Tuple idx val) ->
        fromMaybe acc $ List.modifyAt idx (const val) acc
      actualList = RandomAccessList.toList $ updates raList updateList
    actualList `shouldEqual` expectedList
