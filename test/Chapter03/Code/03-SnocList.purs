module Test.Chapter03.Code.SnocList where

import Prelude

import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldl, foldr)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndexDefaultR, foldrWithIndex)
import Data.List (List(..), (:))

-- Same as `List a` but the structure is defined in terms of
-- `init` and `last`. (I first saw this concept
-- when looking at Idris 2's basic types and I was surprised
-- that this type wasn't defined in PureScript.)
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
  foldlWithIndex f b = foldrWithIndex (\i a b' -> f i b' a) b <<< reverse

  foldrWithIndex :: forall a b. (Int -> a -> b -> b) -> b -> SnocList a -> b
  foldrWithIndex = go 0
    where
    go idx f b = case _ of
      SnocNil -> b
      Snoc init last -> go (idx + 1) f (f idx last b) init

  foldMapWithIndex = foldMapWithIndexDefaultR

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
