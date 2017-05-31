module Main where

import Web.Scotty
import Data.Monoid
import Network.HTTP.Types.Status (notFound404)

main = scotty 3000 $ do
  get  "/"      $ html "hello there"
  get  "/hello" $ html "Hello world"
  post "/hello" $ html "Hello postman"
  get  "/hello/:name" $ do
    name <- param "name"
    html $ "Hello " `mappend` name 
