module Test.Chapter03.Code.SymmetricList where

import Prelude

import Data.Eq (class Eq1)
import Data.Foldable (foldl, foldr)
import Data.List (List(..))
import Test.Chapter01.Exercise12.SnocList (SnocList(..), (<:))

-- Same as `CaseSymList` but inlines the `NonEmpty`, 
-- so we don't pay for additional boxing.
-- This is the type we'll use for implementing the rest of the SymmetricList exercises.
--
-- Again, one improvement we could make is to store the size of each list inside the `Ends`
-- constructor. I haven't done so here so as to follow the general idea from the book.
--    e.g. `Ends a Int (List a) Int (SnocList a) a`
data SymmetricList a
  = Empty
  | Single a
  | Ends a (List a) (SnocList a) a

toList :: forall a. SymmetricList a -> List a
toList = case _ of
  Empty ->
    Nil
  Single a ->
    Cons a Nil
  Ends head init tail last ->
    {-
      let lastElem = Cons last Nil
      let back = foldr Cons lastElem tail
      let tail = foldr Cons back init
      Cons head tail
    -}
    Cons head $ foldr Cons (foldr Cons (Cons last Nil) tail) init

-- Notice that we traverse the list only 1 time, unlike the previous approaches.
fromList :: forall a. List a -> SymmetricList a
fromList ls = do
  let
    snoc :: SymmetricList a -> a -> SymmetricList a
    snoc csl a = case csl of
      Empty -> Single a
      Single b -> Ends b Nil SnocNil a
      Ends head init tail last -> Ends head init (tail <: last) a

  foldl snoc Empty ls

instance Show a => Show (SymmetricList a) where
  show = case _ of
    Empty -> "Empty"
    Single a -> "(Single " <> show a <> " )"
    Ends h i t l -> "(Ends " <> show h <> " " <> show i <> " " <> show t <> " " <> show l <> " )"

derive instance Eq a => Eq (SymmetricList a)
derive instance Eq1 SymmetricList
