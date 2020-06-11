module GeoJSON.GeocodingService
    ( GeocodingService
    , serviceOSMNominatim
    , serviceMapTiler
    , serviceMapBox
    , serviceAdresseGouvFr
    )
where

import Prelude

import Affjax (Request, defaultRequest)
import Affjax.ResponseFormat (json)
import Data.Argonaut (Json)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe, maybe)
import Web.URL as URL
import Web.URL.URLSearchParams as USP


type GeocodingService = String -> Request Json


-- | Open Street Map Nominatim
-- Use with `geocodeAddressNominatim`.
-- Specify an optional language.
-- See: https://nominatim.openstreetmap.org/
serviceOSMNominatim :: Maybe String -> GeocodingService
serviceOSMNominatim language location =
    let url = URL.unsafeFromAbsolute "https://nominatim.openstreetmap.org/search"
            # URL.setSearch (USP.toString
                $ USP.fromString "?format=geojson&addressdetails=1"
                # USP.append "q" location
                # maybe identity (USP.append "accept-language") language)
    in  mkServiceRequest url

-- MapTiler
-- Specify the key and an optional language.
-- See: https://cloud.maptiler.com/geocoding/
serviceMapTiler :: String -> Maybe String -> GeocodingService
serviceMapTiler key language location =
    let url = URL.unsafeFromRelative (location <> ".json") "https://api.maptiler.com/geocoding/"
            # URL.setSearch (USP.toString
                $ USP.fromString ""
                # USP.append "key" key
                # maybe identity (USP.append "language") language)
    in  mkServiceRequest url

-- MapBox
-- Specify the key and an optional language.
-- See: https://docs.mapbox.com/api/search/
serviceMapBox :: String -> Maybe String -> GeocodingService
serviceMapBox key language location =
    let url = URL.unsafeFromRelative (location <> ".json") "https://api.mapbox.com/geocoding/v5/mapbox.places/"
            # URL.setSearch (USP.toString
                $ USP.fromString ""
                # USP.append "access_token" key
                # maybe identity (USP.append "language") language)
    in  mkServiceRequest url

-- | API Adresse
-- Note: Addresses in France only!
-- See: https://geo.api.gouv.fr/adresse
serviceAdresseGouvFr :: GeocodingService
serviceAdresseGouvFr location =
    let url = URL.unsafeFromAbsolute "https://api-adresse.data.gouv.fr/search/"
            # URL.setSearch (USP.toString
                $ USP.fromString ""
                # USP.append "q" location)
    in  mkServiceRequest url


mkServiceRequest :: URL.URL -> Request Json
mkServiceRequest url = defaultRequest
    { url = URL.toString url
    , method = Left GET
    , responseFormat = json
    , headers = []
    }
