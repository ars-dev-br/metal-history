{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module MH.Data.Band
  ( BandResult(..)
  , Band(..)
  , BandAlbum(..)
  ) where

import Data.Aeson
import Data.Text

data BandResult = BandResult
  { status :: Text
  , code :: Int
  , message :: Text
  , bandData :: Maybe Band
  } deriving Show

data Band = Band
  { id :: Text
  , name :: Text
  , discography :: [BandAlbum]
  } deriving Show

data BandAlbum = BandAlbum
  { title :: Text
  , id :: Text
  , albumType :: Text
  , year :: Text
  } deriving Show

instance FromJSON BandResult where
  parseJSON = withObject "BandResult" $ \v -> BandResult
    <$> v .: "status"
    <*> v .: "code"
    <*> v .: "message"
    <*> v .: "data"

instance FromJSON Band where
  parseJSON = withObject "Band" $ \v -> Band
    <$> v .: "id"
    <*> v .: "band_name"
    <*> v .: "discography"

instance FromJSON BandAlbum where
  parseJSON = withObject "BandAlbum" $ \v -> BandAlbum
    <$> v .: "title"
    <*> v .: "id"
    <*> v .: "type"
    <*> v .: "year"
