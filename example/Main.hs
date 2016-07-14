{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}

import           Data.Proxy
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Servant.API
import           Servant.Route

type API = 
  "home" :> QueryParam "heh" Text :> Post '[JSON] [Int] :<|>
  "about":> Capture "foo" Int :> Get '[JSON] [Int]

-- data Route = Home !Text | About !Int, generated from TH

$(getRoute (Proxy :: Proxy API) id)

main :: IO ()
main = undefined
