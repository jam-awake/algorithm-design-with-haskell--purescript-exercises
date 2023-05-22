module Test.Chapter03.Spec where

import Prelude

import Test.Chapter03.Exercise01 as Exercise01
import Test.Chapter03.Exercise02 as Exercise02
import Test.Chapter03.Exercise03 as Exercise03
import Test.Chapter03.Exercise04 as Exercise04
import Test.Chapter03.Exercise05 as Exercise05
import Test.Chapter03.Exercise06 as Exercise06
import Test.Chapter03.Exercise10 as Exercise10
import Test.Chapter03.Exercise11 as Exercise11
import Test.Chapter03.Exercise12 as Exercise12
-- import Test.Chapter03.ExerciseX as ExerciseX
import Test.Spec (Spec, describe)

spec :: Spec Unit
spec = describe "Chapter 3" do
  Exercise01.spec
  Exercise02.spec
  Exercise03.spec
  Exercise04.spec
  Exercise05.spec
  Exercise06.spec
  Exercise10.spec
  Exercise11.spec
  Exercise12.spec
  -- ExerciseX.spec
  pure unit
