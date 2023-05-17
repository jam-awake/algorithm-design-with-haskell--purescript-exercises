module Test.Chapter03.Spec where

import Prelude

import Test.Chapter03.Exercise01 as Exercise01
-- import Test.Chapter03.ExerciseX as ExerciseX
import Test.Spec (Spec, describe)

spec :: Spec Unit
spec = describe "Chapter 2" do
  Exercise01.spec
  -- ExerciseX.spec
  pure unit
