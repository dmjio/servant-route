{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Proxy
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Servant.API
import           Servant.Route
import           Data.Char

class IsRoute a where
  parseRoute :: T.Text -> Either T.Text a
  toRoute :: a -> T.Text

type API = "home" :> Capture "bar" Int :> "fun" :> Get '[JSON] NoContent
      :<|> "about" :> Capture "z" Int :> Capture "z" Int :> Get '[JSON] NoContent
      :<|> Get '[JSON] NoContent

main :: IO ()
main = putStrLn "main"

$(getRoute (Proxy :: Proxy API) (filter isAlpha))


