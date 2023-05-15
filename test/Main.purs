module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Chapter01.Spec as Chapter01
import Test.Chapter02.Spec as Chapter02
-- import Test.ChapterX.Spec as ChapterX
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  Chapter01.spec
  Chapter02.spec
-- ChapterX.spec
  pure unit
