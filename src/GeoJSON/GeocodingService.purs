module GeoJSON.GeocodingService
    ( GeocodingService
    , serviceOSMNominatim
    , serviceMapTiler
    , serviceMapBox
    , serviceAdresseGouvFr
    , ServiceURL
    , parseServiceURL
    , printServiceURL
    , addQueryPair
    , addPath
    )
where

import Prelude

import Affjax (Request, defaultRequest)
import Affjax.ResponseFormat (json)
import Data.Argonaut (Json)
import Data.Array (unsnoc, (:))
import Data.Either (Either(..), fromRight, hush)
import Data.HTTP.Method (Method(..))
import Data.Lens (over)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Text.Parsing.Parser (runParser)
import URI.AbsoluteURI (AbsoluteURI, AbsoluteURIOptions, HierPath, Host, Path(..), Port, UserInfo)
import URI.AbsoluteURI as URI
import URI.Extra.QueryPairs (QueryPairs(..))
import URI.Extra.QueryPairs as QP
import URI.HostPortPair (HostPortPair)
import URI.HostPortPair as HPP
import URI.Path.Segment as PS

type ServiceURL = AbsoluteURI UserInfo (HostPortPair Host Port) Path HierPath (QueryPairs String String)

type GeocodingService = String -> Request Json


-- | Open Street Map Nominatim
-- Use with `geocodeAddressNominatim`.
-- Specify an optional language.
-- See: https://nominatim.openstreetmap.org/
serviceOSMNominatim :: Maybe String -> GeocodingService
serviceOSMNominatim language location =
    let url = unsafeParseServiceURL "https://nominatim.openstreetmap.org/search?format=geojson&addressdetails=1"
            # addQueryPair "q" location
            # maybe identity (addQueryPair "accept-language") language
    in  mkServiceRequest url

-- MapTiler
-- Specify the key and an optional language.
-- See: https://cloud.maptiler.com/geocoding/
serviceMapTiler :: String -> Maybe String -> GeocodingService
serviceMapTiler key language location =
    let url = unsafeParseServiceURL "https://api.maptiler.com/geocoding/"
            # addPath (location <> ".json")
            # addQueryPair "key" key
            # maybe identity (addQueryPair "language") language
    in  mkServiceRequest url

-- MapBox
-- Specify the key and an optional language.
-- See: https://docs.mapbox.com/api/search/
serviceMapBox :: String -> Maybe String -> GeocodingService
serviceMapBox key language location =
    let url = unsafeParseServiceURL "https://api.mapbox.com/geocoding/v5/mapbox.places/"
            # addPath (location <> ".json")
            # addQueryPair "access_token" key
            # maybe identity (addQueryPair "language") language
    in  mkServiceRequest url

-- | API Adresse
-- Note: Addresses in France only!
-- See: https://geo.api.gouv.fr/adresse
serviceAdresseGouvFr :: GeocodingService
serviceAdresseGouvFr location =
    let url = unsafeParseServiceURL "https://api-adresse.data.gouv.fr/search/"
            # addQueryPair "q" location
    in  mkServiceRequest url


mkServiceRequest :: ServiceURL -> Request Json
mkServiceRequest url = defaultRequest
    { url = printServiceURL url
    , method = Left GET
    , responseFormat = json
    , headers = []
    }

addQueryPair :: String -> String -> ServiceURL -> ServiceURL
addQueryPair key value = over (URI._query) addQP
  where
    addQP xs = Just (QueryPairs (Tuple key (Just value) : maybe [] (\(QueryPairs xs') -> xs') xs))

addPath :: String -> ServiceURL -> ServiceURL
addPath path = over (URI._hierPart <<< URI._path) addPath'
  where
    addPath' (Path xs) = Path $
        case unsnoc xs of
            Just { init, last }
                | last == PS.segmentFromString "" -> init <> [PS.segmentFromString path]
                | otherwise -> xs <> [PS.segmentFromString path]
            Nothing -> [PS.segmentFromString path]

parseServiceURL :: String -> Maybe ServiceURL
parseServiceURL url = hush $ runParser url (URI.parser options)

printServiceURL :: ServiceURL -> String
printServiceURL = URI.print options

unsafeParseServiceURL :: String -> ServiceURL
unsafeParseServiceURL url = unsafePartial $ fromRight $ runParser url (URI.parser options)

options :: Record (AbsoluteURIOptions UserInfo (HostPortPair Host Port) Path HierPath (QueryPairs String String))
options =
    { parseUserInfo: pure
    , printUserInfo: identity
    , parseHosts: HPP.parser pure pure
    , printHosts: HPP.print identity identity
    , parsePath: pure
    , printPath: identity
    , parseHierPath: pure
    , printHierPath: identity
    , parseQuery: QP.parse (Right <<< QP.keyToString) (Right <<< QP.valueToString)
    , printQuery: QP.print QP.keyFromString QP.valueFromString
    }
