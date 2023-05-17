module Test.Chapter03.Exercise06 where

import Prelude

import Test.Spec (Spec, describe, pending')

spec :: Spec Unit
spec = describe "Exercise 6" do
  pending' "initsSL is not heap-safe in a strict language. We need laziness here." do
    pure unit
