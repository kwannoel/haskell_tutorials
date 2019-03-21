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

randomElement :: String -> IO Char
randomElement xs = do
  let maxIndex :: Int
      maxIndex = length xs - 1
  randomDigit <- SR.randomRIO (0, maxIndex)
  return (xs !! randomDigit)

shortyGen :: IO [Char]
shortyGen =
  replicateM 7 (randomElement alphaNum)

saveURI :: R.Connection
        -> BC.ByteString
        -> BC.ByteString
        -> IO (Either R.Reply R.Status)
saveURI conn shortURI uri =
  R.runRedis conn $ R.set shortURI uri

getURI :: R.Connection
       -> BC.ByteString
       -> IO (Either R.Reply
               (Maybe BC.ByteString))
getURI conn shortURI =
  R.runRedis conn $ R.get shortURI

linkShorty :: String -> String
linkShorty shorty =
  concat
  [ "<a href=\""
  , shorty
  , "\">Copy and paste your short URL</a>"]

shortyCreated :: Show a
              => a
              -> String
              -> TL.Text

shortyCreated resp shawty =
  TL.concat [ TL.pack (show resp)
            , " shorty is: "
            , TL.pack (linkShorty shawty)
            ]

shortyAintUri :: TL.Text -> TL.Text
shortyAintUri uri =
  TL.concat
    [ uri
    , " wasn't a url,"
    , " did you forget http://?"
    ]

shortyFound :: TL.Text -> TL.Text
shortyFound tbs =
  TL.concat
    [ "<a href=\""
    , tbs, "\">"
    , tbs, "</a>" ]

app :: R.Connection
    -> ScottyM ()
app rConn = do
--   [1]
  get "/" $ do
--    [2]
    uri <- param "uri"
--          [3]
-- [1] Redis connection that needs to be up before app starts
-- [2] get takes a RoutePattern. RoutePattern has an IsString instance so that the
-- pattern can be a String literal
-- [3] the param function is a means of extracting parameters. param can get
-- parameters from the path, POST requests, and via query params. Because there's
-- no :uri param on the path, we will use query params

-- param :: Parsable a => Data.Text.Internal.Lazy.Text -> ActionM a
-- param takes a Text value, and returns an action

    let parsedUri :: Maybe URI
        parsedUri = parseURI (TL.unpack uri)
    case parsedUri of
--        [1]
      Just _ -> do
        shawty <- liftIO shortyGen
--                [2]
        let shorty = BC.pack shawty
--                   [3]
            uri' = encodeUtf8 (TL.toStrict uri)
--                        [4]

        -- get a storedUri if there is one
        storedUri <- liftIO (getURI rConn shorty)

        -- now check if the shorty exists
        case storedUri of
          -- if we have an error or failing case
          Left reply ->
            -- send the client a text response
            text (TL.pack (show reply))
          -- if we don't have a failure from Redis
          Right mbBS -> case mbBS of
                          -- check if Redis returned a URI
                          -- if it didn't, then the key is available for us to store
                          -- the shortUrl with the original URL
                          Nothing -> do
                            -- so we save the shortUrl and uri in Redis
                            resp <- liftIO (saveURI rConn shorty uri')
--                                       [5]
                            -- and we respond with the html response we get from
                            -- shortyCreated
                            html (shortyCreated resp shawty)
--                            [6]
                          -- otherwise Redis has alreay used the key, and we respond
                          -- with text indicating that the short URI is already
                          -- taken
                          Just _ -> text "shortened URI already exists"
      Nothing -> text (shortyAintUri uri)
--              [7]

-- [1] test if the user provided a valid URI. We use the network-uri library's
-- parseUri function to do this. We're only concerned with whether it was valid or
-- not, so we don't use the value when evaluating Just
-- [2] the monad here is ActionM, an alias of ActionT. ActionT handles requests and
-- returns responses. IO actions can be performed on this Monad, but need to be
-- lifted over the additional structure
-- [3] BC.pack converts our generated shortcode into Char8ByteString for storage
-- in Redis
-- [4] convert the uri provided by the user from a lazy Text value to strict Text
-- value. Then encode as a UTF-8 ByteString for redis
-- [5] use liftIO so we can perform an IO action inside a scotty ActionM. We are
-- saving the shortcode and uri in Redis so it can be looked up. The shortcode is
-- the key, and uri is the value
-- [6] this is the templated response we send the client when a short url is
-- successfully saved
-- [7] this is the error response we send the client

  get "/:short" $ do
--      [1]
    short <- param "short"
--            [2]
    uri <- liftIO (getURI rConn short)
--              [3]
    case uri of
      Left reply ->
--     [4]
        text (TL.pack (show reply))
--         [5]
      Right mbBS -> case mbBS of
--      [6]
                      Nothing -> text "uri not found"
--                      [7]
                      Just bs -> html (shortyFound tbs)
--                      [8]
                        where tbs :: TL.Text
                              tbs = TL.fromStrict (decodeUtf8 bs)

main :: IO ()
main = do
  rConn <- R.connect R.defaultConnectInfo
  scotty 3000 (app rConn)
{-
param :: Parsable a
      => Data.Text.Internal.Lazy.Text
      -> ActionM
-}
