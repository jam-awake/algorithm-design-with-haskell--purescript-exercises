module Test.Chapter03.Code.SnocList where

import Prelude

import Data.Foldable (class Foldable, foldl, foldr)
import Data.List (List(..), (:))

-- Same as `List a` but the structure is defined in terms of
-- `init` and `last`. (I first saw this concept
-- when looking at Idris 2's basic types and I was surprised
-- that this type wasn't defined in PureScript.)
data SnocList a
  = SnocNil
  | Snoc (SnocList a) a

derive instance Eq a => Eq (SnocList a)
derive instance Ord a => Ord (SnocList a)
derive instance Functor SnocList

snocNil :: forall a. SnocList a
snocNil = SnocNil

snoc :: forall a. SnocList a -> a -> SnocList a
snoc = Snoc

infixl 6 Snoc as <:

-- The derived `foldl` would not be stack-safe whereas the derived `foldr` would be.
instance Foldable SnocList where
  foldl :: forall a b. (b -> a -> b) -> b -> SnocList a -> b
  foldl f b list {-
    = case list of
        SnocNil -> 
          b
        Snoc init last ->
          -- Stack unsafe! We need an accumulator here.
          f (foldl f b init) last

    -- For the accumulator value to work, we need to run code like
    --     foldl f (f b head) tail
    -- which is `List`'s `foldl` implementation. Thus,
    -- we should "reverse" a `SnocList` by converting it to a `List`
    -- and then use `foldl`.
    -} = foldl f b $ toList list

  -- This, however, is stack-safe. If we look at `List`'s `foldr` instance,
  -- we'll see that it's `foldr f b <<< toSnocList`.
  foldr :: forall a b. (a -> b -> b) -> b -> SnocList a -> b
  foldr f b = case _ of
    SnocNil -> b
    Snoc init last -> foldr f (f last b) init

  foldMap f = foldr (\a m -> (f a) <> m) mempty

toList :: forall a. SnocList a -> List a
toList = go Nil -- same as `foldr Cons Nil`
  where
  go acc = case _ of
    SnocNil -> acc
    Snoc init last -> go (last : acc) init

fromList :: forall a. List a -> SnocList a
fromList = foldr (flip Snoc) SnocNil
