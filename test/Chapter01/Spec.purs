module Test.Chapter01.Spec where

import Prelude

import Test.Chapter01.Exercise02 as Exercise02
import Test.Chapter01.Exercise03 as Exercise03
import Test.Chapter01.Exercise04 as Exercise04
import Test.Chapter01.Exercise05 as Exercise05
import Test.Chapter01.Exercise06 as Exercise06
import Test.Chapter01.Exercise07 as Exercise07
import Test.Chapter01.Exercise08 as Exercise08
import Test.Chapter01.Exercise11 as Exercise11
import Test.Chapter01.Exercise12 as Exercise12
import Test.Chapter01.Exercise13 as Exercise13
import Test.Chapter01.Exercise15 as Exercise15
import Test.Chapter01.Exercise21 as Exercise21
import Test.Chapter01.Exercise20 as Exercise20
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
  Exercise11.spec
  Exercise12.spec
  Exercise13.spec
  Exercise15.spec
  Exercise21.spec
  Exercise20.spec
  -- ExerciseX.spec
  pure unit
