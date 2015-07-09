{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Handler.Warp
import Servant
--import Servant.Docs
import Servant.HTML.Lucid
import Lucid.Base
import Lucid.Html5

instance ToHtml Int where
  toHtml = h1_ . p_ . toHtml . show

newtype Input t = Input t
instance ToHtml (Input t) where
  toHtml (Input t) = input_ [makeAttribute "type" "text", makeAttribute "name" "firstname", makeAttribute "value" "John"] -- $ toHtml t

type NumberAPI = "obtainnumber" :> Get '[HTML] Int
            :<|> "form" :> Get '[HTML] (Input Int)
            :<|> "add" :> Capture "x" Int :> Capture "x" Int :> Get '[HTML] Int

serveNumber :: Server NumberAPI
serveNumber =    return 42
            :<|> (return $ Input 25)
            :<|> (\ x y -> return (x + y))

--instance ToCapture (Capture "x" Int) where
--  toCapture _ = DocCapture "x" "an integer to add"

--instance ToSample Int where
--  toSample = Just 0

main :: IO ()
main = do
  putStrLn "start serving"
  --putStrLn $ markdown (docs (Proxy :: Proxy NumberAPI))
  run 8080 (serve (Proxy :: Proxy NumberAPI) serveNumber)
