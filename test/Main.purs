module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
-- import Test.ChapterX.Spec as ChapterX
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  -- ChapterX.spec
  pure unit
