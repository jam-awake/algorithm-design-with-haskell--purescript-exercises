module Test.Chapter03.Code.CaseSymList where

import Prelude

import Data.Foldable (foldl, foldr)
import Data.List (List(..))
import Data.NonEmpty (NonEmpty, (:|))
import Test.Chapter03.Code.SnocList (SnocList(..), (<:))

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

-- Notice that we traverse the list only 1 time, unlike the previous approaches.
fromList :: forall a. List a -> CaseSymList a
fromList ls = do
  let
    snoc :: CaseSymList a -> a -> CaseSymList a
    snoc csl a = case csl of
      Empty -> Single a
      Single b -> Ends (b :| Nil) (a :| SnocNil)
      Ends front (last :| tail) -> Ends front (a :| tail <: last)

  foldl snoc Empty ls
