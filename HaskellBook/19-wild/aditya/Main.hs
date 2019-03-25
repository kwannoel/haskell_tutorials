module AdityaDb where

import Web.Scotty

runDb :: SqlPersist (ResourceT IO) a
      -> IO a
runDb query = do
  let connStr :: String
      connStr =
        -- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
        foldr (\(k, v) t ->
                 t <> (encodeUtf8 $
                       k <> "=" <> v <> " "))
              "" params
  runResourceT
    . withPostgresqlConn connStr
    $ runSqlConn query
