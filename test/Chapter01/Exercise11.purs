module Test.Chapter01.Exercise11 where

import Prelude

import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.List (List(..), foldr, (:))
import Data.List as List
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

integer :: List Int -> Int
integer = _.int <<< flip foldr { decimal: 1, int: 0 } \next acc ->
  { decimal: acc.decimal * 10
  , int: acc.int + (next * acc.decimal)
  }

fraction :: List Int -> Number
fraction = _.num <<< flip foldr { decimal: 10, num: 0.0 } \next acc ->
  { decimal: acc.decimal * 10
  , num: acc.num + (toNumber next / toNumber acc.decimal)
  }

spec :: Spec Unit
spec = describe "Exercise 11" do
  let
    mkInput msg input expected = { msg, input, expected }
    inputs =
      [ mkInput "empty" (Nil :: List Int) 0
      , mkInput "1" (1 : Nil) 1
      , mkInput "1..9" (List.range 1 9) 123_456_789
      ]
  for_ inputs \r ->
    it ("integer - list is" <> r.msg) do
      integer r.input `shouldEqual` r.expected
