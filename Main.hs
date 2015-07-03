{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Main where

import Network.Wai.Handler.Warp
import Servant
--import Servant.Docs
import Servant.HTML.Lucid
import Lucid.Base
import Lucid.Html5

instance ToHtml Int where
  --toHtml = True
  toHtml _ = toHtml "Hello!"
--  toHtmlRaw _i = h1_ $ p_ "Hello!"

type NumberAPI = "obtainnumber" :> Get '[HTML] Int
            :<|> "add" :> Capture "x" Int :> Capture "x" Int :> Get '[HTML] Int

serveNumber :: Server NumberAPI
serveNumber =    return 42
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
