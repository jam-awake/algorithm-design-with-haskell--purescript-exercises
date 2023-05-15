module Test.Chapter02.Spec where

import Prelude

import Test.Chapter02.Exercise01 as Exercise01
-- import Test.Chapter02.ExerciseX as ExerciseX
import Test.Spec (Spec, describe)

spec :: Spec Unit
spec = describe "Chapter 2" do
  Exercise01.spec
  -- ExerciseX.spec
  pure unit
