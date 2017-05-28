{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}

module Lib
    ( someFunc
    ) where

import Control.Exception (throwIO)
import Data.Aeson
import Data.Text
import Network.HTTP.Req

import qualified HM.Data.Album as A
import qualified HM.Data.Band as B
import qualified HM.Data.Search as S

instance MonadHttp IO where
  handleHttpException = throwIO

getAlbum :: (MonadHttp m) => Text -> m A.AlbumResult
getAlbum id = do
  r <- request $ albumUrl id
  return (responseBody r :: A.AlbumResult)
    where
      albumUrl :: Text -> Url 'Http
      albumUrl = (http "em.wemakesites.net" /: "album" /:)

getBand :: (MonadHttp m) => Text -> m B.BandResult
getBand id = do
  r <- request $ bandUrl id
  return (responseBody r :: B.BandResult)
    where
      bandUrl :: Text -> Url 'Http
      bandUrl = (http "em.wemakesites.net" /: "band" /:)

search :: (MonadHttp m) => Text -> m S.SearchResult
search keyword = do
  r <- request $ searchUrl keyword
  return (responseBody r :: S.SearchResult)
    where
      searchUrl :: Text -> Url 'Http
      searchUrl = (http "em.wemakesites.net" /: "search" /: "band_name" /: )

apiKey :: Option scheme
apiKey = "api_key" =: ("" :: Text)

request :: (MonadHttp m, FromJSON a) => Url scheme -> m (JsonResponse a)
request url = req GET url NoReqBody jsonResponse apiKey

someFunc :: IO ()
someFunc = do
  r <- getBand "689"
  print r
