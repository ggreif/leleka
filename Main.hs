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
import Test.QuickCheck
import Control.Applicative

import Data.Text.Lazy (toStrict)
import qualified Data.Text as T
import Control.Monad.IO.Class

data MathML = Number Integer
            | MathML `Plus` MathML
            | MathML `Minus` MathML
            | MathML `Times` MathML
            | MathML `QuotRem` MathML
  deriving Show

instance Num MathML where
  fromInteger = Number
  (+) = Plus
  (-) = Minus
  (*) = Times

instance Real MathML where
instance Eq MathML where
  Number a == Number b = a == b
instance Ord MathML where
  Number a < Number b = a < b
instance Enum MathML where
instance Integral MathML where
  quot = QuotRem
  --quotRem = QuotRem

isHard (Number a `Plus` Number b) = a < 0 || a > 99 || b < 0 || b > 99 || a + b > 100
isHard (Number a `Minus` Number b) = a < b || a < 0 || a > 99 || b < 0 || b > 99
isHard (Number a `Times` Number b) = a < 0 || a > 99 || b < 0 || b > 99 || a * b > 100
isHard (Number a `QuotRem` Number b) = a < 0 || a > 99 || b <= 0 || b > 11
isHard _ = False

isSimple (0 `Plus` _) = True
isSimple (_ `Plus` 0) = True
isSimple (0 `Times` _) = True
isSimple (_ `Times` 0) = True
isSimple (1 `Times` _) = True
isSimple (_ `Times` 1) = True
isSimple (_ `Minus` 0) = True
isSimple (_ `QuotRem` 1) = True
isSimple _ = False


instance Arbitrary MathML where
  arbitrary = frequency [ (1, Plus <$> arbI <*> arbI)
                        , (2, Minus <$> arbI <*> arbI)
                        , (1, Times <$> arbI <*> arbI)
                        , (3, Times <$> arb11 <*> arb11)
                        , (4, QuotRem <$> arbI <*> arbDivisor)
                        ]
    where arbI = Number <$> resize 100 arbitrarySizedNatural
          arbDivisor = Number <$> resize 20 arbitrarySizedNatural
          arb11 = Number <$> resize 11 arbitrarySizedNatural

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
                                      mo_ "\247"
                                      toHtml b

instance ToHtml Int where
  toHtml = h1_ . p_ . toHtml . show

newtype Input t = Input t
instance {-# OVERLAPPABLE #-} Show t => ToHtml (Input t) where
  toHtml (Input t) = input_ [type_ "text", name_ "inp", value_ $ toStrict $ renderText $ toHtml $ show t]

instance {-# OVERLAPPING #-} Show t => ToHtml (Input (Maybe t)) where
  toHtml (Input Nothing) = input_ [type_ "text", name_ "inp"]
  toHtml (Input (Just a)) = toHtml (Input a)

instance {-# OVERLAPPING #-} ToHtml (Input ()) where
  toHtml (Input t) = input_ [type_ "submit"]


instance {-# OVERLAPPABLE #-} (ToHtml t, ToHtml u) => ToHtml (t, u) where
  toHtml (t, u) = toHtml t >> toHtml u

instance {-# OVERLAPPING #-} ToHtml u => ToHtml (MathML, u) where
  toHtml (t, u) = math_ (mrow_ (toHtml t >> mpadded_ [width_ "+2em"] (mo_ "="))) >> toHtml u

type NumberAPI = "obtainnumber" :> Get '[HTML] Int
            :<|> "math" :> Get '[HTML] MathML
            :<|> "mathx" :> QueryParams "inp" Int :> Get '[HTML] (Form ([(MathML, Input (Maybe Int))], Input ()))
            :<|> "form" :> Get '[HTML] (Input Int)
            :<|> "formPair" :> QueryParam "inp" Int :> Get '[HTML] (Form (Input Int, Input ()))
            :<|> "add" :> Capture "x" Int :> Capture "x" Int :> Get '[HTML] Int
            :<|> "random" :> Get '[HTML] ([Int])
            :<|> "simple" :> Get '[HTML] ([(MathML, Input (Maybe Int))])

instance ToHtml t => ToHtml [t] where
  toHtml [] = return ()
  toHtml (t:ts) = do toHtml t
                     br_ []
                     toHtml ts

serveNumber :: Server NumberAPI
serveNumber =    return 42
            :<|> (return $ 1 * (8 + 1) - 5 `quot` 77)
            :<|> (\is -> return . Form . (, Input ()) $ map inputize $ zip (maybeize is) [23*2, 4 `quot` 1, 7+4])
            :<|> (return $ Input 25)
            :<|> biform
            :<|> (\ x y -> return (x + y))
            :<|> (liftIO $ generate $ vector 40)
            :<|> (fmap (fmap resultize) $ liftIO $ generate $ vectorOf 30 $ arbitrary `suchThat` (not . isHard))
  where biform Nothing = return $ Form (Input 25, Input ())
        biform (Just n) = return $ Form (Input (n + 1), Input ())
        inputize (val, a) = (a, Input val)
        resultize = (, Input Nothing)
        maybeize = foldr (\s ms -> Just s : ms) $ repeat Nothing

newtype Form a = Form a
instance ToHtml t => ToHtml (Form t) where
  toHtml (Form t) = do form_ [method_ "get"] $ do toHtml t


main :: IO ()
main = do
  putStrLn "start serving"
  run 8080 (serve (Proxy :: Proxy NumberAPI) serveNumber)
