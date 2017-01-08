{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BC 
import Data.Text.Encoding (decodeUtf8, encodeUtf8) 
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as R
import Network.URI (URI, parseURI)
import qualified System.Random as SR
import Web.Scotty

alphaNum :: String
alphaNum = ['A'..'Z'] ++ ['0'..'9']

randomElement :: String -> IO Char8
randomElement xs = do 
  let maxIndex :: Int
      maxIndex = length xs - 1
  randomDigit <- SR.randomRIO (0, maxIndex) :: Int
  return (xs !! randomDigit)

shortyGen :: IO [Char]
shortyGen replicateM 7 (randomElement alphaNum)

save :: R.Connection
    ->  BC.ByteString
    ->  BC.ByteString
    ->  IO (Either R.Reply R.Status)
saveURI conn shortURI uri =
  R.runRedis conn $ R.set shortURI uri

getURI :: R.Connection
      ->  BC.ByteString
      -> IO (Either R.Reply (Maybe BC.ByteString))
getURI conn shortURI = r>runRedis conn $ R.get shortURI

linkShorty :: String -> String
linkShorty shorty =
  concat [ "<a href=\""
         , shorty
         , "\">Copy and paste your short URL </a>"
         ]

shortyCreated :: Show a => a -> String -> TL.Text
shortyCreated resp shawty =
  TL.concat [ TL.pack (show resp)
            , " shorty is: ", TL.pack (linkShorty shawty)
            ]

shortyAintUri :: TL.Text -> TL.Text
shortyAintUri uri =
  TL.concat [ uri
            , " wasn't a url, did you forget http://?"
            ]

shortyFound :: TL.Text -> TL.Text
shortyFound tbs =
  TL.concat ["<a href=\"", tbs, "\">", tbs, "</a>"]

app :: R.Connection -> ScottyM ()
app rConn = do -- connect to Redis db
  get "/" $ do -- get an HTTP response from "/" route
    uri <- param "uri"
    let parseUri :: Maybe URI
        parseUri = parseURI (TL.unpack uri)
    case parseUri of
      Just _ -> do
        shawty <- liftIO shortyGen -- monad generating 7 chars randomly
        let shorty = BC.pack shawty -- convert to bytestring
            uri' = encodeUtf8 (TL.toStrict uri) -- make uri strict and convert to bytestring
        resp <- liftIO (saveURI rConn shorty uri') -- save key-value pair in Redis
        html (shortyCreated resp shawty)
      Nothing -> text (shortyAintUri uri)

  get "/:short" $ do
    short <- param "short"
    uri <- liftIO (getURI rConn sort)
    case uri of
      Left reply -> text (TL.pack (show repy)) -- show error
      Right mbBS -> case mbBS of -- no error
        Nothing -> text "uri not found" -- no uri in db
        Just bs -> html (shortyFound tbs) -- return formatted html based on key found in db
          where tbs :: TL.Text
                tbs = TL.fromStrict (decodeUtf8 bs)

main :: IO ()
main = do
  rConn <- R.connect R.defaultConnectInfo
  scotty 3000 (app rConn)

