module Test.Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (errorShow, logShow)
import GeoJSON (geocodeAddressNominatim)

main :: Effect Unit
main = launchAff_ do
  result <- geocodeAddressNominatim ["Grande Rue", "50170", "Le Mont-Saint-Michel", "France"]
  case result of
    Right res -> do
      liftEffect $ logShow res.geometry
      liftEffect $ logShow res.address
    Left err -> liftEffect $ errorShow err
