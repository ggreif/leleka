{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Main where

import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Lucid
import Lucid.Base
import Lucid.Html5
import Lucid.MathML

import Data.Text.Lazy

data MathML = MathML `Times` MathML

instance Num MathML where
  (*) = Times

instance ToHtml MathML where
  toHtml ml = table_ [rows_ "2"]
                     (tr_ $ do td_ [class_ "top", colspan_ "2", style_ "color:red"]
                                   (p_ "Hello, attributes!")
                               td_ "yay!")
              >> toHtml ("hhH" :: Text)


instance ToHtml Int where
  toHtml = h1_ . p_ . toHtml . show

newtype Input t = Input t
instance Show t => ToHtml (Input t) where
  toHtml (Input t) = input_ [makeAttribute "type" "text", makeAttribute "name" "firstname", makeAttribute "value" $ toStrict $ renderText $ toHtml $ show t]

instance (ToHtml (Input t), ToHtml (Input u)) => ToHtml (Input t, Input u) where
  toHtml (t, u) = toHtml t >> toHtml u

type NumberAPI = "obtainnumber" :> Get '[HTML] Int
            :<|> "math" :> Get '[HTML] MathML
            :<|> "form" :> Get '[HTML] (Input Int)
            :<|> "formPair" :> Get '[HTML] (Input Int, Input Int)
            :<|> "add" :> Capture "x" Int :> Capture "x" Int :> Get '[HTML] Int

serveNumber :: Server NumberAPI
serveNumber =    return 42
            :<|> (return $ 1 * 1)
            :<|> (return $ Input 25)
            :<|> (return (Input 25, Input 42))
            :<|> (\ x y -> return (x + y))


main :: IO ()
main = do
  putStrLn "start serving"
  --putStrLn $ markdown (docs (Proxy :: Proxy NumberAPI))
  run 8080 (serve (Proxy :: Proxy NumberAPI) serveNumber)
