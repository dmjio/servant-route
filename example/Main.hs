{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}

import           Data.Proxy
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           GHC.Ptr
import           GHC.TypeLits
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Servant.API
import           Servant.Route

type API = 
  "home" :> QueryParam "heh" Int :> Post '[JSON] () :<|>
  "about":> Capture "foo" Int :> Get '[JSON] [Int]

$(getRoute (Proxy :: Proxy API) id)


