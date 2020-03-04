module GeoJSON.Geocode
    ( GeocodingError(..)
    , NominatimResult
    , Address(..)
    , geocodeAddressNominatim
    , geocodeLocation
    , geocodeLocation'
    )
where

import Prelude

import Affjax (Error(..), printError, request)
import Data.Argonaut (Json, (.:), (.:?))
import Data.Argonaut as A
import Data.Array (filter, head, intercalate, null)
import Data.Either (Either(..), note)
import Data.Foldable (oneOf)
import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import GeoJSON.GeoJSON (Feature, Geometry, decodeFeatureCollection)
import GeoJSON.GeocodingService (GeocodingService)


data GeocodingError
    = DeserialisationError String
    | UnknownError String
    | NoResultsError

instance showGeocodingError :: Show GeocodingError where
    show = case _ of
        DeserialisationError x -> "Could not deserialize GeoJSON: " <> show x
        UnknownError x -> "Unknown error: " <> x
        NoResultsError -> "No results for query"

type NominatimResult =
    { geometry   :: Geometry
    , properties :: Json
    , address    :: Address
    }

type Address =
    { house_number   :: Maybe String
    , road           :: Maybe String
    , city           :: Maybe String
    , hamlet         :: Maybe String
    , postcode       :: Maybe String
    , county         :: Maybe String
    , state_district :: Maybe String
    , state          :: Maybe String
    , region         :: Maybe String
    , island         :: Maybe String
    , country        :: Maybe String
    , country_code   :: Maybe String
    , continent      :: Maybe String
    }

geocodeAddressNominatim :: GeocodingService -> Array String -> Aff (Either GeocodingError NominatimResult)
geocodeAddressNominatim h location = do
    result <- geocodeLocation h location
    case result of
      Right feature -> do
        case decodeAddressNominatim feature.properties of
          Right address ->
            pure $ Right
                { geometry: feature.geometry
                , properties: feature.properties
                , address: address
                }
          Left str -> pure (Left (DeserialisationError ("Nominatim: " <> str)))
      Left err -> pure (Left err)


-- Decode an address as specified in the Nominatim API. We map several keys to a
-- simpler model. This is not perfect, but I think at least slightly more useful
-- than the original.
decodeAddressNominatim :: Json -> Either String Address
decodeAddressNominatim json = do
    r <- A.decodeJson json
    x <- r .: "address"
    postcode <- x .:? "postcode"
    state_district <- x .:? "state_district"
    region <- x .:? "region"
    island <- x .:? "island"
    country_code <- x .:? "country_code"
    continent <- x .:? "continent"
    house_number <- oneOf <$> traverse (x .:? _) ["house_number", "street_number"]
    road <- oneOf <$> traverse (x .:? _) [
        "road", "footway", "street", "street_name", "residential", "path",
        "pedestrian", "road_reference", "road_reference_intl", "square", "place",
        "cycleway"]
    city <- oneOf <$> traverse (x .:? _) ["city", "village", "town", "municipality"]
    hamlet <- oneOf <$> traverse (x .:? _) ["hamlet", "locality", "croft"]
    county <- oneOf <$> traverse (x .:? _) ["county", "local_administrative_area", "county_code"]
    state <- oneOf <$> traverse (x .:? _) ["state", "province", "state_code"]
    country <- oneOf <$> traverse (x .:? _) ["country", "country_name"]
    pure {
        house_number, road, city, hamlet, county, postcode, state_district,
        state, region, island, country, country_code, continent
    }


geocodeLocation :: GeocodingService -> Array String -> Aff (Either GeocodingError Feature)
geocodeLocation h location = do
    r <- geocodeLocation' h location
    case r of
        Right rs -> pure (note NoResultsError (head rs))
        Left err -> pure (Left err)

geocodeLocation' :: GeocodingService -> Array String -> Aff (Either GeocodingError (Array Feature))
geocodeLocation' h location = do
    let location' = intercalate ", " (filter (_ /= "") location)
    res <- request (h location')
    case res of
        Right resp -> do
            case decodeFeatureCollection resp.body of
                Right x
                    | null x.features -> pure (Left NoResultsError)
                    | otherwise -> pure (Right x.features )
                Left err -> pure (Left (DeserialisationError (show err)))
        Left (ResponseBodyError err _) -> pure (Left (DeserialisationError (show err)))
        Left err -> pure (Left (UnknownError (printError err)))
