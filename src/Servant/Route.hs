{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds           #-}
module Servant.Route (HasRoute(..), getRoute, empty', defOptions) where

import Control.Applicative
import Data.Char
import Data.Proxy
import Data.Text                         (Text)
import Data.Typeable
import GHC.TypeLits
import Language.Haskell.TH
import Servant.API
import Servant.API.ContentTypes

class HasRoute api where
  route :: Proxy api -> (String, [Type]) -> (String -> String) -> [Con]

getRoute :: HasRoute api => Proxy api -> (String -> String) -> Q [Dec]
getRoute p f = pure [ DataD [] (mkName "Route") [] (route p empty' f) [] ]

instance (Typeable typ, HasRoute api, KnownSymbol name) =>
  HasRoute (Capture name typ :> api) where
    route Proxy (n,xs) f = route (Proxy :: Proxy api) (n, xs ++ [typ]) f
     where
       typ = ConT . mkName . show . typeRep $ (Proxy :: Proxy typ)

instance (Typeable typ, HasRoute api, KnownSymbol name) =>
  HasRoute (QueryParam name typ :> api) where
    route Proxy (n,xs) f = route (Proxy :: Proxy api) (n, xs ++ [typ]) f
      where
        typ = ConT . mkName . show . typeRep $ (Proxy :: Proxy typ)

instance (HasRoute api, KnownSymbol name) =>
  HasRoute (QueryFlag name :> api) where
    route Proxy (n,xs) f = route (Proxy :: Proxy api) (n, xs) f

instance (HasRoute l, HasRoute r) => HasRoute (l :<|> r) where
    route Proxy (n,xs) f =
      route (Proxy :: Proxy l) (n, xs) f <|>
        route (Proxy :: Proxy r) (n, xs) f

instance (KnownSymbol sym, HasRoute api) =>
  HasRoute (sym :> api) where
    route Proxy (n,xs) f = route (Proxy :: Proxy api) (n ++ nam, xs) f
      where
        nam = capitalize $ symbolVal (Proxy :: Proxy sym)

instance HasRoute api => HasRoute (HttpVersion :> api) where
    route Proxy (n,xs) f = route (Proxy :: Proxy api) (n, xs) f  

instance HasRoute api => HasRoute (Vault :> api) where
    route Proxy (n,xs) f = route (Proxy :: Proxy api) (n, xs) f  

instance HasRoute api => HasRoute (IsSecure :> api) where
    route Proxy (n,xs) f = route (Proxy :: Proxy api) (n, xs) f  

instance HasRoute api => HasRoute (RemoteHost :> api) where
    route Proxy (n,xs) f = route (Proxy :: Proxy api) (n, xs) f  

instance (Typeable typ, HasRoute api, KnownSymbol name) =>
  HasRoute (Header name typ :> api) where
    route Proxy (n,xs) f = route (Proxy :: Proxy api) (n, xs) f

instance ( AllCTUnrender list a, HasRoute api)
  => HasRoute (ReqBody list a :> api) where
    route Proxy (n,xs) f = route (Proxy :: Proxy api) (n, xs) f

instance {-# overlappable #-}
  ( AllCTRender ctypes a, ReflectMethod method, KnownNat status
  ) => HasRoute (Verb method status ctypes a) where
    route Proxy (n,xs) f = [ NormalC nam typs ]
      where
        typs = map (IsStrict,) xs
        nam = mkName $ capitalize (f n)
        capitalize [] = []
        capitalize (x:xs) = toUpper x : xs

instance {-# overlapping #-}
         ( AllCTRender ctypes a, ReflectMethod method, KnownNat status
         , GetHeaders (Headers h a)
         ) => HasRoute (Verb method status ctypes (Headers h a)) where
    route Proxy (n,xs) f = [ NormalC nam typs ]
      where
        typs = map (IsStrict,) xs
        nam = mkName $ capitalize (f n)

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

defOptions :: String -> String
defOptions = id

empty' :: ([a], [b])
empty' = (,) [] []
