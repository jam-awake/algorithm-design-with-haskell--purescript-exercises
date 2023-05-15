module Test.Chapter02.Code.AdtSymList where

import Prelude

import Data.Foldable (class Foldable, foldr)
import Data.List (List(..))

-- Further simplifies the book's type to something that is much easier
-- to implement.
-- One can start to see the beginning of the Finger Tree data structure here.
-- in that the `a`s currently store only one value, but could be updated
-- to store 2 or 3 values in various contexts.
data AdtSymList a
  = Empty
  | Single a
  | Ends a (AdtSymList a) a

derive instance Functor AdtSymList
derive instance Foldable AdtSymList

toList :: forall a. AdtSymList a -> List a
toList = case _ of
  Empty -> Nil
  Single a -> Cons a Nil
  Ends head middle last ->
    Cons head $ foldr Cons (Cons last Nil) middle
