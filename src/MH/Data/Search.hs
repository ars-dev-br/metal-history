{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module MH.Data.Search
  ( SearchResult(..)
  , SearchData(..)
  , Band(..)
  ) where

import Data.Aeson
import Data.Text
import GHC.Generics

data SearchResult = SearchResult
  { status :: Text
  , code :: Int
  , message :: Text
  , searchData :: Maybe SearchData
  } deriving Show

data SearchData = SearchData
  { query :: Text
  , bands :: [Band]
  } deriving Show

data Band = Band
  { name :: Text
  , id :: Text
  , genre :: Text
  , country :: Text
  } deriving (Generic, Show)

instance FromJSON SearchResult where
  parseJSON = withObject "SearchResult" $ \v -> SearchResult
    <$> v .: "status"
    <*> v .: "code"
    <*> v .: "message"
    <*> v .: "data"

instance FromJSON SearchData where
  parseJSON = withObject "SearchData" $ \v -> SearchData
    <$> v .: "query"
    <*> v .: "search_results"

instance FromJSON Band
