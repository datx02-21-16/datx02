module Main where

import Prelude
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import GUI as G

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI G.siteBody unit body
