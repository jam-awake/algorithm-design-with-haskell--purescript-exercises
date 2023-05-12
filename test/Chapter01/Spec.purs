module Test.Chapter01.Spec where

import Prelude

import Test.Chapter01.Exercise02 as Exercise02
import Test.Chapter01.Exercise03 as Exercise03
import Test.Chapter01.Exercise04 as Exercise04
import Test.Chapter01.Exercise05 as Exercise05
import Test.Chapter01.Exercise06 as Exercise06
import Test.Chapter01.Exercise07 as Exercise07
import Test.Chapter01.Exercise08 as Exercise08
-- import Test.Chapter01.ExerciseX as ExerciseX
import Test.Spec (Spec, describe)

spec :: Spec Unit
spec = describe "Chapter 1" do
  Exercise02.spec
  Exercise03.spec
  Exercise04.spec
  Exercise05.spec
  Exercise06.spec
  Exercise07.spec
  Exercise08.spec
  -- ExerciseX.spec
  pure unit
