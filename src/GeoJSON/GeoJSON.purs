-- Implementation of GeoJSON
-- See: https://tools.ietf.org/html/rfc7946

module GeoJSON.GeoJSON where

import Prelude

import Data.Argonaut (Json, (.:), (:=), (~>))
import Data.Argonaut as A
import Data.Array (head, tail)
import Data.Either (Either(..), note)
import Data.Lens (_Left, over)
import Data.Traversable (traverse)


type FeatureCollection = { features :: Array Feature }

type Feature = { properties :: Json, geometry :: Geometry }

type Coordinate = { x :: Number, y :: Number}

data Geometry
    = Point Coordinate
    | LineString (Array Coordinate)
    -- TODO
    -- | Polygon (Array Coordinate) (Array Coordinate)
    -- | MultiPoint (Array Coordinate)
    -- | MultiLineString (Array (Array Coordinate))
    -- | MultiPolygon (Array (Array (Array Coordinate)))

instance showGeometry :: Show Geometry where
  show = case _ of
    Point { x, y } -> "(Point " <> show x <> " " <> show y <> ")"
    LineString _ -> "(LineString)"

data GeoJsonError
    = InvalidFeatureCollection
    | InvalidFeature
    | InvalidGeometry
    | InvalidCoordinate String
    | DecodeJsonError String

instance showGeoJsonError :: Show GeoJsonError where
    show InvalidFeatureCollection = "Invalid FeatureCollection"
    show InvalidFeature = "Invalid Feature"
    show InvalidGeometry = "Invalid Geometry"
    show (InvalidCoordinate x) = "Invalid Coordinate: " <> x
    show (DecodeJsonError x) = "JSON error: " <> x

encodeFeatureCollection :: FeatureCollection -> Json
encodeFeatureCollection c@{ features } =
    "type" := "FeatureCollection"
        ~> "features" := (encodeFeature <$> features)
        ~> A.jsonEmptyObject

decodeFeatureCollection :: Json -> Either GeoJsonError FeatureCollection
decodeFeatureCollection json = do
    x <- mapError $ A.decodeJson json
    t <- mapError $ x .: "type"
    case t of
        "FeatureCollection" -> do
            xs <- mapError $ x .: "features"
            features <- traverse decodeFeature xs
            pure { features }
        _ -> Left InvalidFeatureCollection

encodeFeature :: Feature -> Json
encodeFeature f@{ properties, geometry } =
    "type" := "Feature"
        ~> "geometry" := encodeGeometry geometry
        ~> "properties" := properties
        ~> A.jsonEmptyObject

decodeFeature :: Json -> Either GeoJsonError Feature
decodeFeature json = do
    x <- mapError $ A.decodeJson json
    t <- mapError $ x .: "type"
    case t of
        "Feature" -> do
            geometry <- decodeGeometry =<< mapError (x .: "geometry")
            properties <- mapError <$> A.decodeJson =<< mapError (x .: "properties")
            pure { geometry, properties }
        _ -> Left InvalidFeature

encodeCoordinate :: Coordinate -> Json
encodeCoordinate { x, y } = A.encodeJson [x, y]

decodeCoordinate :: Json -> Either GeoJsonError Coordinate
decodeCoordinate json = do
    cs <- mapError $ A.decodeJson json
    x <- note (InvalidCoordinate "Could not parse X coordinate") (head cs)
    y <- note (InvalidCoordinate "Could not parse Y coordinate") (head =<< tail cs)
    pure { x, y }

encodeGeometry :: Geometry -> Json
encodeGeometry = case _ of
    (Point c) ->
        "type" := "Point"
        ~> "coordinates" := encodeCoordinate c
        ~> A.jsonEmptyObject
    (LineString cs) ->
        "type" := "LineString"
        ~> "coordinates" := (encodeCoordinate <$> cs)
        ~> A.jsonEmptyObject

decodeGeometry :: Json -> Either GeoJsonError Geometry
decodeGeometry json = do
    x <- mapError $ A.decodeJson json
    t <- mapError $ x .: "type"
    case t of
        "Point" -> do
            cs <- mapError $ x .: "coordinates"
            c <- decodeCoordinate cs
            pure (Point c)
        _ -> Left InvalidGeometry

mapError :: forall a. Either String a -> Either GeoJsonError a
mapError = over _Left DecodeJsonError
