module Test.Chapter01.Exercise12.SnocList where

import Prelude

import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldl, foldr)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndexDefaultL, foldlWithIndex)
import Data.List (List(..), (:))

-- Whereas `List a` is defined in terms of `head` and `tail`,
-- `SnocList` is defined in terms of `init` and `last`. 
-- (I first saw this concept when looking at Idris 2's basic types
-- and I was surprised that this type wasn't defined in PureScript.)
--
-- ```
-- let list     =            1  : 2  : 3 : Nil
-- let snocList = SnocNil <: 1 <: 2 <: 3
-- toList snocList == list && fromList lsit == snocList
-- ```
data SnocList a
  = SnocNil
  | Snoc (SnocList a) a

derive instance Eq a => Eq (SnocList a)
derive instance Eq1 SnocList
derive instance Ord a => Ord (SnocList a)
derive instance Functor SnocList

snocNil :: forall a. SnocList a
snocNil = SnocNil

snoc :: forall a. SnocList a -> a -> SnocList a
snoc = Snoc

infixl 6 Snoc as <:

instance Show a => Show (SnocList a) where
  show ls = go "" ls
    where
    go acc = case _ of
      SnocNil
        | acc == "" -> "SnocNil"
        | otherwise -> "(SnocNil" <> acc <> ")"
      Snoc rest a -> go (acc <> " <: " <> show a) rest

-- The derived `foldl` would not be stack-safe whereas the derived `foldr` would be.
instance Foldable SnocList where
  foldl :: forall a b. (b -> a -> b) -> b -> SnocList a -> b
  foldl f b list = foldr (flip f) b $ reverse list

  -- This, however, is stack-safe. If we look at `List`'s `foldr` instance,
  -- we'll see that it's `foldr f b <<< toSnocList`.
  foldr :: forall a b. (a -> b -> b) -> b -> SnocList a -> b
  foldr f b = case _ of
    SnocNil -> b
    Snoc init last -> foldr f (f last b) init

  foldMap f = foldr (\a m -> (f a) <> m) mempty

instance FoldableWithIndex Int SnocList where
  foldlWithIndex :: forall a b. (Int -> b -> a -> b) -> b -> SnocList a -> b
  foldlWithIndex f b = foldlWithIndex f b <<< toList

  -- Unfortunately, `foldrWithIndex` assumes the index is zero-based in a left-to-right direction.
  -- For `SnocList`, it would be ideal if the index was in a right-to-left direction.
  -- So, we have to reverse the list to get the total number of elements
  -- and then traverse the list a second time to do the fold, producing the index
  -- by subtracting the current index from the length
  foldrWithIndex :: forall a b. (Int -> a -> b -> b) -> b -> SnocList a -> b
  foldrWithIndex f b ls = go 0 b ls
    where
    lastIdx = length ls - 1
    go offset b' = case _ of
      SnocNil -> b
      Snoc init last -> go (offset + 1) (f (lastIdx - offset) last b') init

  foldMapWithIndex = foldMapWithIndexDefaultL

-- Similar to `foldrWithIndex` but the `Int` arg is in a right-to-left direction
-- where the last element has index 0 and the first element has (`SnocList.length ls - 1`).
foldrWithLastIndex :: forall a b. (Int -> a -> b -> b) -> b -> SnocList a -> b
foldrWithLastIndex f = go 0
  where
  go lastIdx b = case _ of
    SnocNil -> b
    Snoc init last -> go (lastIdx + 1) (f lastIdx last b) init

-- Converts the structure without changing the order of elements.
toList :: forall a. SnocList a -> List a
toList = go Nil -- same as `foldr Cons Nil`
  where
  go acc = case _ of
    SnocNil -> acc
    Snoc init last -> go (last : acc) init

-- Converts the structure without changing the order of elements.
fromList :: forall a. List a -> SnocList a
fromList = foldl Snoc SnocNil

reverse :: forall a. SnocList a -> SnocList a
reverse = foldr (flip Snoc) SnocNil

length :: forall a. SnocList a -> Int
length = foldr (\_ acc -> acc + 1) 0
