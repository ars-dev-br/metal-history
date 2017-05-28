{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Lib
    ( someFunc
    ) where

import           Control.Exception (throwIO)
import           Data.List
import           Data.Maybe
import           Data.Ord
import qualified Data.Text as T
import           Data.Time
import           Network.HTTP.Req

import qualified MH.Data.Album as A
import qualified MH.Data.Band as B
import qualified MH.Data.Search as S
import           MH.Net

instance MonadHttp IO where
  handleHttpException = throwIO

apiKey :: T.Text
apiKey = ""

sortedAlbums :: [T.Text] -> IO [A.AlbumData]
sortedAlbums names = do
  searchResults <- mapM (search apiKey) names
  bandResults <- mapM fromSearchToBand searchResults
  albumResults <- mapM fromBandToAlbums bandResults
  return $ sortAlbumResults (concat albumResults)

fromSearchToBand :: S.SearchResult -> IO B.BandResult
fromSearchToBand searchResult =
  let searchData = fromJust $ S.searchData searchResult
      searchBand = head $ S.bands searchData in
    getBand apiKey (S.id searchBand)

fromBandToAlbums :: B.BandResult -> IO [A.AlbumResult]
fromBandToAlbums bandResult =
  let bandData = fromJust $ B.bandData bandResult
      discography = B.discography bandData
      fullLength = filter (\a -> B.albumType a == "Full-length") discography in
    mapM (getAlbum apiKey . albumId) fullLength
  where
    albumId :: B.BandAlbum -> T.Text
    albumId = B.id

sortAlbumResults :: [A.AlbumResult] -> [A.AlbumData]
sortAlbumResults = sortBy compareAlbumData . map (fromJust . A.albumData)

compareAlbumData :: A.AlbumData -> A.AlbumData -> Ordering
compareAlbumData lhs rhs =
  comparing (timeFromString . A.releaseDate . A.album) lhs rhs

timeFromString :: T.Text -> Maybe UTCTime
timeFromString s = listToMaybe . catMaybes $ ([st, nd, rd, th] <*> pure s)
  where
    st = parseTimeM True defaultTimeLocale "%B %est, %Y" . T.unpack
    nd = parseTimeM True defaultTimeLocale "%B %end, %Y" . T.unpack
    rd = parseTimeM True defaultTimeLocale "%B %erd, %Y" . T.unpack
    th = parseTimeM True defaultTimeLocale "%B %eth, %Y" . T.unpack

someFunc :: IO ()
someFunc = do
  albums <- sortedAlbums [ "Judas Priest"
                         , "Black Sabbath"
                         , "Deep Purple"
                         , "Thin Lizzy"
                         , "Motorhead"
                         ]
  mapM_ (putStrLn . show) $
    map (\a -> (A.name $ A.band a, A.title $ A.album a, A.releaseDate $ A.album a)) albums
