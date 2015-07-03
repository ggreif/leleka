{-# LANGUAGE OverloadedStrings, DataKinds, TypeOperators, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Main where

import Network.Wai.Handler.Warp
import Servant
--import Servant.Docs
--import Servant.Client

data Weekday = Monday | Tuesday | Wednesday | Thursday

type NumberAPI = "obtainnumber" :> Get '[JSON] Int
            :<|> "add" :> Capture "x" Int :> Capture "x" Int :> Get '[JSON] Int

serveNumber :: Server NumberAPI
serveNumber =    return 42
            :<|> (\ x y -> return (x + y))

--instance ToCapture (Capture "x" Int) where
--  toCapture _ = DocCapture "x" "an integer to add"

--instance ToSample Int where
--  toSample = Just 0

--obtainnumber :<|> add = client (Proxy :: Proxy NumberAPI)

main :: IO ()
main = do
  --putStrLn $ markdown (docs (Proxy :: Proxy NumberAPI))
  run 8080 (serve (Proxy :: Proxy NumberAPI) serveNumber)
