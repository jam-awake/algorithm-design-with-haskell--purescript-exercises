module Test.Chapter03.Code.CaseSymList where

import Prelude

import Data.List (List(..))
import Data.Foldable (foldr)
import Data.NonEmpty (NonEmpty, (:|))
import Test.Chapter03.Code.SnocList (SnocList)

-- Simplifies the invariants described by the book with an ADT
-- that highlights the three different cases.
data CaseSymList a
  = Empty
  | Single a
  | Ends (NonEmpty List a) (NonEmpty SnocList a)

toList :: forall a. CaseSymList a -> List a
toList = case _ of
  Empty ->
    Nil
  Single a ->
    Cons a Nil
  Ends (head :| init) (last :| tail) ->
    {-
      let lastElem = Cons last Nil
      let back = foldr Cons lastElem tail
      let tail = foldr Cons back init
      Cons head tail
    -}
    Cons head $ foldr Cons (foldr Cons (Cons last Nil) tail) init