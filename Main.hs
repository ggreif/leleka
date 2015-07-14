{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

-- 7.8??
{-# LANGUAGE OverlappingInstances #-}

module Main where

import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Lucid
import Lucid.Base
import Lucid.Html5
import Lucid.MathML

import Data.Text.Lazy hiding (map)
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
  toHtml (a `QuotRem` b) = mrow_ $ do toHtml a
                                      mo_ "÷"
                                      toHtml b

instance ToHtml Int where
  toHtml = h1_ . p_ . toHtml . show

newtype Input t = Input t
instance {-# OVERLAPPABLE #-} Show t => ToHtml (Input t) where
  toHtml (Input t) = input_ [type_ "text", name_ "firstname", value_ $ toStrict $ renderText $ toHtml $ show t]

instance {-# OVERLAPPING #-} Show t => ToHtml (Input (Maybe t)) where
  toHtml (Input Nothing) = input_ [type_ "text", name_ "firstname"]
  toHtml (Input (Just a)) = toHtml (Input a)

instance {-# OVERLAPPING #-} ToHtml (Input ()) where
  toHtml (Input t) = input_ [type_ "submit"]


instance (ToHtml t, ToHtml u) => ToHtml (t, u) where
  toHtml (t, u) = toHtml t >> toHtml u

type NumberAPI = "obtainnumber" :> Get '[HTML] Int
            :<|> "math" :> Get '[HTML] MathML
            :<|> "mathx" :> QueryParams "firstname" Int :> Get '[HTML] (Form ([(MathML, Input (Maybe Int))], Input ()))
            :<|> "form" :> Get '[HTML] (Input Int)
            :<|> "formPair" :> QueryParam "firstname" Int :> Get '[HTML] (Form (Input Int, Input ()))
            :<|> "add" :> Capture "x" Int :> Capture "x" Int :> Get '[HTML] Int

instance ToHtml t => ToHtml [t] where
  toHtml [] = return ()
  toHtml (t:ts) = do toHtml t
                     br_ []
                     toHtml ts


serveNumber :: Server NumberAPI
serveNumber =    return 42
            :<|> (return $ 1 * (8 + 1) - 5 `quot` 77)
            :<|> (\is -> return . Form . (, Input ()) $ map inputize [23*2, 4 `quot` 1, 7+4])
            :<|> (return $ Input 25)
            :<|> biform
            :<|> (\ x y -> return (x + y))
  where biform Nothing = return $ Form (Input 25, Input ())
        biform (Just n) = return $ Form (Input (n + 1), Input ())
        inputize a = (a, Input Nothing)

newtype Form a = Form a
instance ToHtml t => ToHtml (Form t) where
  toHtml (Form t) = do form_ [method_ "get"] $ do toHtml t


main :: IO ()
main = do
  putStrLn "start serving"
  run 8080 (serve (Proxy :: Proxy NumberAPI) serveNumber)
