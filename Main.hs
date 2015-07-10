{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Main where

import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Lucid
import Lucid.Base
import Lucid.Html5

import Data.Text.Lazy

instance ToHtml Int where
  toHtml = h1_ . p_ . toHtml . show

newtype Input t = Input t
instance Show t => ToHtml (Input t) where
  toHtml (Input t) = input_ [makeAttribute "type" "text", makeAttribute "name" "firstname", makeAttribute "value" $ toStrict $ renderText $ toHtml $ show t]

instance (ToHtml (Input t), ToHtml (Input u)) => ToHtml (Input t, Input u) where
  toHtml (t, u) = toHtml t >> toHtml u

type NumberAPI = "obtainnumber" :> Get '[HTML] Int
            :<|> "form" :> Get '[HTML] (Input Int)
            :<|> "formPair" :> Get '[HTML] (Input Int, Input Int)
            :<|> "add" :> Capture "x" Int :> Capture "x" Int :> Get '[HTML] Int

serveNumber :: Server NumberAPI
serveNumber =    return 42
            :<|> (return $ Input 25)
            :<|> (return (Input 25, Input 42))
            :<|> (\ x y -> return (x + y))


main :: IO ()
main = do
  putStrLn "start serving"
  --putStrLn $ markdown (docs (Proxy :: Proxy NumberAPI))
  run 8080 (serve (Proxy :: Proxy NumberAPI) serveNumber)
