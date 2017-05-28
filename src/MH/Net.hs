{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}

module MH.Net
  ( getAlbum
  , getBand
  , search
  ) where

import Data.Aeson
import Data.Text
import Network.HTTP.Req

import qualified MH.Data.Album as A
import qualified MH.Data.Band as B
import qualified MH.Data.Search as S

getAlbum :: (MonadHttp m) => Text -> Text -> m A.AlbumResult
getAlbum key id = do
  r <- request key $ albumUrl id
  return (responseBody r :: A.AlbumResult)
    where
      albumUrl :: Text -> Url 'Http
      albumUrl = (http "em.wemakesites.net" /: "album" /:)

getBand :: (MonadHttp m) => Text -> Text -> m B.BandResult
getBand key id = do
  r <- request key $ bandUrl id
  return (responseBody r :: B.BandResult)
    where
      bandUrl :: Text -> Url 'Http
      bandUrl = (http "em.wemakesites.net" /: "band" /:)

search :: (MonadHttp m) => Text -> Text -> m S.SearchResult
search key keyword = do
  r <- request key $ searchUrl keyword
  return (responseBody r :: S.SearchResult)
    where
      searchUrl :: Text -> Url 'Http
      searchUrl = (http "em.wemakesites.net" /: "search" /: "band_name" /: )

request :: (MonadHttp m, FromJSON a) => Text -> Url scheme -> m (JsonResponse a)
request key url = req GET url NoReqBody jsonResponse queryKey
  where
    queryKey = "api_key" =: (key :: Text)
