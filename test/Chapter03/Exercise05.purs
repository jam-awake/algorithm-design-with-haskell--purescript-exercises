module Test.Chapter03.Exercise05 where

import Prelude

import Data.Foldable (foldl, for_)
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Test.Chapter03.Code.SnocList (SnocList(..), (<:))
import Test.Chapter03.Code.SnocList as SnocList
import Test.Chapter03.Code.SymmetricList (SymmetricList(..))
import Test.Chapter03.Code.SymmetricList as SymmetricList
import Test.Chapter03.Exercise04 (splitSLInit)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

halveListInit :: forall a. List a -> Maybe { head :: a, init :: List a, tail :: SnocList a }
halveListInit ls = do
  let
    { len, snocList } = foldl (\acc next -> { len: acc.len + 1, snocList: acc.snocList <: next }) { len: 0, snocList: SnocNil } ls
    midPoint = len / 2
  splitSLInit midPoint snocList

dropWhileSL :: forall a. (a -> Boolean) -> SymmetricList a -> SymmetricList a
dropWhileSL predicate ls = case ls of
  Empty -> ls
  Single a
    | predicate a -> Empty
    | otherwise -> ls
  Ends head init tail last
    | predicate head -> do
        case List.uncons $ List.dropWhile predicate init of
          Just initR -> Ends initR.head initR.tail tail last
          Nothing -> case halveListInit $ List.dropWhile predicate $ SnocList.toList tail of
            Just tailR -> Ends tailR.head tailR.init tailR.tail last
            Nothing
              | predicate last -> Empty
              | otherwise -> Single last
    | otherwise -> ls

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
    describe msg do
      it "dropWhile x is even" do
        let p x = x `mod` 2 == 0
        List.dropWhile p i `shouldEqual` (SymmetricList.toList $ dropWhileSL p symList)
      it "dropWhile x < 4" do
        let p x = x < 4
        List.dropWhile p i `shouldEqual` (SymmetricList.toList $ dropWhileSL p symList)
      it "dropWhile x == 0" do
        let p x = x == 0
        List.dropWhile p i `shouldEqual` (SymmetricList.toList $ dropWhileSL p symList)
