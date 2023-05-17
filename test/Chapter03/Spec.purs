module Test.Chapter03.Spec where

import Prelude

import Test.Chapter03.Exercise01 as Exercise01
import Test.Chapter03.Exercise02 as Exercise02
import Test.Chapter03.Exercise03 as Exercise03
import Test.Chapter03.Exercise04 as Exercise04
-- import Test.Chapter03.ExerciseX as ExerciseX
import Test.Spec (Spec, describe)

spec :: Spec Unit
spec = describe "Chapter 3" do
  Exercise01.spec
  Exercise02.spec
  Exercise03.spec
  Exercise04.spec
  -- ExerciseX.spec
  pure unit
