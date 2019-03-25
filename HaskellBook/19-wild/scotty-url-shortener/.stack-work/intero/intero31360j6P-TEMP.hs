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

-- Extracts a random element from a String
randomElement :: String -> IO Char
randomElement xs = do
  let maxIndex :: Int
      maxIndex = length xs - 1
  -- from a range of integers get a digit
  randomDigit <- SR.randomRIO (0, maxIndex)
  return (xs !! randomDigit)

shortyGen :: IO [Char]
shortyGen =
  (replicateM :: Int -> IO Char -> IO [Char]) 7 (randomElement alphaNum)

saveURI :: R.Connection
        -> BC.ByteString
        -> BC.ByteString
        -> IO (Either R.Reply R.Status)

-- The idea here is to cache the short and long URI as a key-value pair

saveURI conn shortURI uri =
  R.runRedis conn $ R.set shortURI uri

-- Retrieval of the long URL if it exists
getURI :: R.Connection
       -> BC.ByteString
       -> IO (Either R.Reply
               (Maybe BC.ByteString))

getURI conn shortURI =
  R.runRedis conn $ R.get shortURI

linkShorty :: String -> String
linkShorty shorty =
  concat
  [ "<a href =\""
  , shorty
  , "\">Copy and paste your short URL</a>"
  ]

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
  -- root of server e.g. http://localhost
  -- get :: RoutePattern -> ActionM() -> ScottyM()
  -- RoutePattern has an isString instance for Overloaded Strings
  get "/" $ do
    -- param :: Parsable a => Text -> ActionM a
    uri <- param "uri"
    -- getting query args e.g. www.asd.com/?<URIquery>
    let parsedUri :: Maybe URI
    --  [1] check for a valid URI by using parseURI function
        parsedUri =
          --parseURI :: String -> Maybe URI
          -- TL.unpack :: Text -> String
          parseURI (TL.unpack uri)
    case parsedUri of
      Just _ -> do
        -- liftIO :: IO a -> m a
        -- passing the return value not the structure
        shawty <- liftIO shortyGen -- reminder that shortyGen :: IO [Char]
  --              [2] the Monad here is ActionM, a dataType representing code that handles req and res
        -- BC.pack :: String -> Text
        let shorty = BC.pack shawty
  --                    [3] process of storing the generated short code for the URI into a ByteString in Redis
            uri' =
              -- convert Lazy.Text ---> Text ---> ByteString
              encodeUtf8 (TL.toStrict uri)
        --    [4]
        resp <-
          liftIO (saveURI rConn shorty uri')
  --      [5] liftIO to perform an IO action inside a scotty ActionM
  --      liftIO :: IO a -> m a
  --      saveURI :: connection -> BC -> BC -> IO (Either Reply (Maybe BC))
        --html :: Text -> ActionM ()
        --shortyCreated :: Show a => a -> String -> TL.Text
        -- resp :: Show a => a (In this case a is probably Either Reply (Maybe BC))
        html (shortyCreated resp shawty)
  --    [6] templated URI to share
      -- text :: Lazy.Text -> ActionM
      Nothing -> text (shortyAintUri uri)
  --             [7] Invalid URI
  get "/:short" $ do
   -- [1] capturing get requests to this endpoint

    short <- param "short"
  --         [2] fetching path capture rather than query arg
    uri <- liftIO (getURI rConn short)
  --       [3] IO action within ActionM getting the short code as the look up key
    case uri of
      Left reply ->
        text (TL.pack (show reply))
  --    [4] Left signifies failure by convention
  --            [5] returns an error since errors have a show instance
      Right mbBS -> case mbBS of
  --  [6] functioning
        Nothing -> text "uri not found"
  --    [7] if key was not in database
        Just bs -> html (shortyFound tbs)
  --    [8] restoring the URI
          where tbs :: TL.Text
                tbs =
                  TL.fromStrict (decodeUtf8 bs)
  --                             [9]

main :: IO ()
main = do
  rConn <- R.connect R.defaultConnectInfo
  scotty 3000 (app rConn)
