module GeoJSON
  ( module GeoJSON.GeoJSON
  , module GeoJSON.Geocode
  , module GeoJSON.GeocodingService
  ) where

import GeoJSON.GeoJSON (Coordinate, Feature, FeatureCollection, GeoJsonError(..), Geometry(..), decodeCoordinate, decodeFeature, decodeFeatureCollection, decodeGeometry, encodeCoordinate, encodeFeature, encodeFeatureCollection, encodeGeometry)
import GeoJSON.Geocode (GeocodingError(..), NominatimResult, geocodeAddressNominatim, geocodeLocation, geocodeLocation')
import GeoJSON.GeocodingService (GeocodingService, serviceOSMNominatim, serviceMapTiler, serviceAdresseGouvFr)
