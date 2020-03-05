module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (errorShow, logShow)
import GeoJSON (geocodeAddressNominatim, geocodeLocation, serviceOSMNominatim, serviceMapTiler, serviceMapBox, serviceAdresseGouvFr)

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

  -- MapTiler
  let mt = serviceMapTiler "wQiskySbRZ1We7tsswDi" (Just "fr")
  resultMT <- geocodeLocation mt location
  case resultMT of
    Right res -> liftEffect $ logShow res.geometry
    Left err -> liftEffect $ errorShow err

  -- MapBox
  let mb = serviceMapBox "pk.eyJ1IjoibWplcHJvbmsiLCJhIjoiY2s1MmpxMGpoMTRvcjNvbXJnMjZ1cnJ6cSJ9.0nfC2fsnJumEtOm1xJcUsw" (Just "fr")
  resultMB <- geocodeLocation mb location
  case resultMB of
    Right res -> liftEffect $ logShow res.geometry
    Left err -> liftEffect $ errorShow err

  -- adresse.gouv.fr
  resultAGF <- geocodeLocation serviceAdresseGouvFr location
  case resultAGF of
    Right res -> liftEffect $ logShow res.geometry
    Left err -> liftEffect $ errorShow err
