module Test.Chapter01.Spec where

import Prelude

import Test.Chapter01.Exercise02 as Exercise02
-- import Test.Chapter01.ExerciseX as ExerciseX
import Test.Spec (Spec, describe)

spec :: Spec Unit
spec = describe "Chapter 1" do
  Exercise02.spec
-- ExerciseX.spec
  pure unit
