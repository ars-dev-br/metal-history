{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module HM.Data.Album
  ( AlbumResult
  ) where

import Data.Aeson
import Data.Text
import GHC.Generics

data AlbumResult = AlbumResult
  { status :: Text
  , code :: Int
  , message :: Text
  , albumData :: Maybe AlbumResultData
  } deriving Show

data AlbumResultData = AlbumResultData
  { band :: Band
  , album :: Album
  } deriving (Generic, Show)

data Band = Band
  { name :: Text
  , id :: Text
  } deriving Show

data Album = Album
  { title :: Text
  , id :: Text
  , albumType :: Text
  , releaseDate :: Text
  } deriving Show

instance FromJSON AlbumResult where
  parseJSON = withObject "AlbumResult" $ \v -> AlbumResult
    <$> v .: "status"
    <*> v .: "code"
    <*> v .: "message"
    <*> v .: "data"

instance FromJSON AlbumResultData

instance FromJSON Band where
  parseJSON = withObject "Band" $ \v -> Band
    <$> v .: "band_name"
    <*> v .: "id"

instance FromJSON Album where
  parseJSON = withObject "Album" $ \v -> Album
    <$> v .: "title"
    <*> v .: "id"
    <*> v .: "type"
    <*> v .: "release date"
