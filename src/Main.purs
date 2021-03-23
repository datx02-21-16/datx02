module Main where

import Prelude
import Effect (Effect)
import GUI as G
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI G.siteBody unit body
