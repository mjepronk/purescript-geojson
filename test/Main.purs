module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (errorShow, logShow)
import GeoJSON (geocodeAddressNominatim, geocodeLocation, serviceOSMNominatim, serviceMapTiler, serviceAdresseGouvFr)

main :: Effect Unit
main = launchAff_ do
  let location = ["Grande Rue", "50170", "Le Mont-Saint-Michel", "France"]

  -- Open Street Map Nominatim
  let osm = serviceOSMNominatim (Just "fr")
  resultOSM <- geocodeAddressNominatim osm location
  case resultOSM of
    Right res -> do
      liftEffect $ logShow res.geometry
      liftEffect $ logShow res.address
    Left err -> liftEffect $ errorShow err

  -- Map Tiler
  let mt = serviceMapTiler "wQiskySbRZ1We7tsswDi"
  resultMT <- geocodeLocation mt location
  case resultMT of
    Right res -> do
      liftEffect $ logShow res.geometry
    Left err -> liftEffect $ errorShow err

  -- adresse.gouv.fr
  resultAGF <- geocodeLocation serviceAdresseGouvFr location
  case resultAGF of
    Right res -> do
      liftEffect $ logShow res.geometry
    Left err -> liftEffect $ errorShow err
