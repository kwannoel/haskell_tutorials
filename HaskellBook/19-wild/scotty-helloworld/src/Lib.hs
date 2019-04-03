{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( helloWorld
    ) where

import Web.Scotty
import Data.Monoid (mconcat)

helloWorld = scotty 3000 $ do
  get "/:word" $ do
    html
      ("<html><body><div class='comments'><div class='comment container'><span class='comment author'>Sally</span><div class='comment text'>Woo hoo!</div></div><div class='comment container'><span class='comment author'>Bill</span><img class='comment image' src='http://example.com/cat.gif' /></div><div class='comment container'><span class='comment author'>Susan</span><div class='comment text'>WTF!?!</div></div></div></body></html>")

-- Just [TextComment "Sally" "Woo hoo!",ImageComment "Bill" "http://example.com/cat.gif",TextComment "Susan" "WTF!?!"]
