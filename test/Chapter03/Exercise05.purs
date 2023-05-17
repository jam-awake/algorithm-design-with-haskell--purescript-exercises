module Test.Chapter03.Exercise05 where

import Prelude

import Data.Foldable (foldl, foldr, for_)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Test.Chapter03.Code.SnocList (SnocList(..), (<:))
import Test.Chapter03.Code.SnocList as SnocList
import Test.Chapter03.Code.SymmetricList (SymmetricList(..))
import Test.Chapter03.Code.SymmetricList as SymmetricList
import Test.Chapter03.Exercise04 (splitListLast, splitSLInit)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

halveListInit :: forall a. List a -> Maybe { head :: a, init :: List a, tail :: SnocList a }
halveListInit ls = do
  let
    convertAndCount acc next =
      { len: acc.len + 1
      , snocList: acc.snocList <: next
      }
    { len, snocList } = foldl convertAndCount { len: 0, snocList: SnocNil } ls
    midPoint = len / 2
  splitSLInit midPoint snocList

dropWhileSL :: forall a. (a -> Boolean) -> SymmetricList a -> SymmetricList a
dropWhileSL predicate ls = case ls of
  Empty -> ls
  Single a | predicate a -> Empty
  Ends head init tail last | predicate head -> do
    case List.dropWhile predicate init of
      Cons head' init' -> Ends head' init' tail last
      Nil ->
        -- There's a lot going on here, so let's break it down
        -- 1. a `SnocList` is in `last` position and the drop direction is from head-to-last.
        --    Since we need to access that head, we convert it to a List via toList
        -- 2. `List` already has a `dropWhile`, so we just reuse that here.
        -- 3. To get the `head`, `init`, and `tail` values, we need to use `halveSLInit`, but
        --    that only works on `SnocList`s, not `List`s. Trying to implement it
        --    for `List`s gets problematic fast due to direction-related issues. 
        --    (If you're curious to see what those are, try implementing this yourself.)
        --    While we could convert the `List` to a `SnocList` and then use `halveSLInit`,
        --    the issue is that `halveSLInit` will traverse the list to get the length of it
        --    to calculate the midpoint. If we're already traversing the list when converting
        --    it from `List` to `SnocList`, we can calculate the length in that same fold
        --    and then use `splitSlInit` directly. Thus, we've reimplemented `halveSLInit`
        --    in a more efficient manner.
        case halveListInit $ List.dropWhile predicate $ SnocList.toList tail of
          Just tailR -> Ends tailR.head tailR.init tailR.tail last
          Nothing
            | predicate last -> Empty
            | otherwise -> Single last
  _ -> ls

dropEndSnoc :: forall a. (a -> Boolean) -> SnocList a -> SnocList a
dropEndSnoc p ls = case ls of
  Snoc init last
    | p last -> dropEndSnoc p init
    | otherwise -> ls
  SnocNil -> ls

halveSLLast :: forall a. SnocList a -> Maybe { init :: List a, tail :: SnocList a, last :: a }
halveSLLast ls = do
  let
    convertAndCount next acc =
      { len: acc.len + 1
      , list: next : acc.list
      }
    { len, list } = foldr convertAndCount { len: 0, list: Nil } ls
    midPoint = len / 2
  splitListLast midPoint list

-- Note: this is not an exercise in the book, but I'm doing the same thing here
-- to illustrate the reversal
dropWhileEndSL :: forall a. (a -> Boolean) -> SymmetricList a -> SymmetricList a
dropWhileEndSL predicate ls = case ls of
  Empty -> ls
  Single a | predicate a -> Empty
  Ends head init tail last | predicate last -> do
    case dropEndSnoc predicate tail of
      Snoc tail' last' -> Ends head init tail' last'
      SnocNil ->
        case halveSLLast $ dropEndSnoc predicate $ SnocList.fromList init of
          Just initR -> Ends head initR.init initR.tail initR.last
          Nothing
            | predicate head -> Empty
            | otherwise -> Single head
  _ -> ls

spec :: Spec Unit
spec = describe "Exercise 5" do
  let
    inputs =
      [ Tuple "Empty list" (Nil :: List Int)
      , Tuple "Singleton list" $ pure 1
      , Tuple "list of 1..10" $ List.range 1 10
      ]
  for_ inputs \(Tuple msg i) -> do
    let symList = SymmetricList.fromList i
    describe "dropWhile" do
      let isEven x = x `mod` 2 == 0
      let lessThan4 x = x < 4
      let equals0 x = x == 0
      describe msg do
        it "dropWhile x is even" do
          let p = isEven
          List.dropWhile p i `shouldEqual` (SymmetricList.toList $ dropWhileSL p symList)
        it "dropWhile x < 4" do
          let p = lessThan4
          List.dropWhile p i `shouldEqual` (SymmetricList.toList $ dropWhileSL p symList)
        it "dropWhile x == 0" do
          let p = equals0
          List.dropWhile p i `shouldEqual` (SymmetricList.toList $ dropWhileSL p symList)

    describe "dropWhileEnd" do
      let listDropWhileEnd p l = List.reverse $ List.dropWhile p $ List.reverse l
      describe msg do
        it "dropWhileEnd x is even" do
          let p x = x `mod` 2 == 0
          listDropWhileEnd p i `shouldEqual` (SymmetricList.toList $ dropWhileEndSL p symList)
        it "dropWhileEnd x < 4" do
          let p x = x < 4
          listDropWhileEnd p i `shouldEqual` (SymmetricList.toList $ dropWhileEndSL p symList)
        it "dropWhileEnd x == 0" do
          let p x = x == 0
          listDropWhileEnd p i `shouldEqual` (SymmetricList.toList $ dropWhileEndSL p symList)

