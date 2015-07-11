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
import qualified Data.Text as T

data MathML = Number Integer
            | MathML `Plus` MathML
            | MathML `Minus` MathML
            | MathML `Times` MathML
            | MathML `QuotRem` MathML

instance Num MathML where
  fromInteger = Number
  (+) = Plus
  (-) = Minus
  (*) = Times

instance Real MathML where
instance Eq MathML where
instance Ord MathML where
instance Enum MathML where
instance Integral MathML where
  quot = QuotRem
  --quotRem = QuotRem

instance ToHtml Integer where
  toHtml = toHtml . show


instance ToHtml MathML where
  toHtml (Number n) = mn_ $ toHtml n
  toHtml (a `Plus` b) = mrow_ $ do toHtml a
                                   mo_ "+"
                                   toHtml b
  toHtml (a `Minus` b) = mrow_ $ do toHtml a
                                    mo_ "-"
                                    toHtml b
  toHtml (a `Times` b) = mrow_ $ do toHtml a
                                    mo_ "*"
                                    toHtml b

instance ToHtml Int where
  toHtml = h1_ . p_ . toHtml . show

newtype Input t = Input t
instance Show t => ToHtml (Input t) where
  toHtml (Input t) = input_ [makeAttribute "type" "text", makeAttribute "name" "firstname", makeAttribute "value" $ toStrict $ renderText $ toHtml $ show t]

--instance (ToHtml (Input t), ToHtml (Input u)) => ToHtml (Input t, Input u) where
--  toHtml (t, u) = toHtml t >> toHtml u
instance (ToHtml t, ToHtml u) => ToHtml (t, u) where
  toHtml (t, u) = toHtml t >> toHtml u

type NumberAPI = "obtainnumber" :> Get '[HTML] Int
            :<|> "math" :> Get '[HTML] MathML
            :<|> "form" :> Get '[HTML] (Input Int)
            :<|> "formPair" :> Get '[HTML] (Input Int, Input Int)
            :<|> "add" :> Capture "x" Int :> Capture "x" Int :> Get '[HTML] Int

serveNumber :: Server NumberAPI
serveNumber =    return 42
            :<|> (return $ 1 * (8 + 1) - 5)
            :<|> (return $ Input 25)
            :<|> (return (Input 25, Input 42))
            :<|> (\ x y -> return (x + y))


main :: IO ()
main = do
  putStrLn "start serving"
  run 8080 (serve (Proxy :: Proxy NumberAPI) serveNumber)
