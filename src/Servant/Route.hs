{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Servant.Route (HasRoute(..), getRoute, defOptions) where

import           Control.Applicative
import           Control.Lens
import           Control.Lens
import           Data.Char
import           Data.Maybe
import           Data.Monoid
import           Data.Monoid
import           Data.Proxy
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Typeable
import           Debug.Trace              (traceShow)
import           GHC.TypeLits
import           Language.Haskell.TH
import           Servant.API
import           Servant.API.ContentTypes
import qualified Data.Text as T
import           Data.Text    (Text)
import           Web.HttpApiData

data RouteADT = RouteADT {
    _rADTName :: String
  , _rADTFields :: [Type]
  } 

data CaptureOrPath = Capture | Path Text

newtype ToRouteInstanceADT = ToRouteInstanceADT {
    _capturesAndPaths :: (String, [CaptureOrPath])
  } 

instance Monoid ToRouteInstanceADT where
  mempty = ToRouteInstanceADT (mempty, mempty)
  ToRouteInstanceADT (n1,c1) `mappend` ToRouteInstanceADT (n2,c2)
    = ToRouteInstanceADT (n1 <> n2, c1 <> c2)

newtype ParseRouteInstanceADT = ParseRouteInstanceADT {
    bar :: [String]
  } deriving Monoid

data RouteInfo = RouteInfo {
    _routeADT :: RouteADT
  , _toRouteInstance :: ToRouteInstanceADT
  , _parseRouteInstance :: ParseRouteInstanceADT
  }

data RouteResult = RouteResult {
    _routeConstructors :: [Con]
  , _routeCapturesAndPaths :: [(String, [CaptureOrPath])]
  } 

$(makeLenses ''ToRouteInstanceADT)
$(makeLenses ''ParseRouteInstanceADT)
$(makeLenses ''RouteInfo)
$(makeLenses ''RouteADT)
$(makeLenses ''RouteResult)
$(makeLenses ''Info)

instance Monoid RouteResult where
  mempty = RouteResult mempty mempty
  r1 `mappend` r2 =
    mempty & routeConstructors .~
              r1 ^. routeConstructors <> r2 ^. routeConstructors
           & routeCapturesAndPaths .~
              r1 ^. routeCapturesAndPaths <> r2 ^. routeCapturesAndPaths

instance Monoid RouteInfo where
  mempty = RouteInfo mempty mempty mempty
  m1 `mappend` m2 =
    RouteInfo (m1 ^. routeADT <> m2 ^. routeADT)
              (m1 ^. toRouteInstance <> m2 ^. toRouteInstance)
              (m1 ^. parseRouteInstance <> m2 ^. parseRouteInstance)

instance Monoid RouteADT where
  mempty = RouteADT mempty mempty 
  m1 `mappend` m2 =
    RouteADT (m1 ^. rADTName <> m2 ^. rADTName)
             (m1 ^. rADTFields <> m2 ^. rADTFields)

type FieldLabelModifier = String -> String

class HasRoute api where
  route :: Proxy api
        -> RouteInfo
        -> FieldLabelModifier
        -> RouteResult 

-- Keep watch for special case

getRoute :: HasRoute api => Proxy api -> (String -> String) -> Q [Dec]
getRoute p f = pure $ constructDeclaration (result ^. routeConstructors)
                   ++ constructInstances (result ^. routeCapturesAndPaths)
  where
    result :: RouteResult
    result = route p mempty f

    constructDeclaration :: [Con] -> [Dec]
    constructDeclaration constructors = 
      [ DataD [] (mkName "Route") [] constructors [] ]

    constructInstances :: [(String, [CaptureOrPath])] -> [Dec]
    constructInstances xs = [ inst ] 
      where
        inst = InstanceD [] (AppT (ConT (mkName "IsRoute")) (ConT (mkName "Route"))) fund
        fund = [ FunD (mkName "toRoute") $ map go xs ]    
        go :: (String, [CaptureOrPath]) -> Clause
        go (name, xs) = 
          Clause [ ConP newName [ VarP $ mkName n | (n, Capture) <- cs ] ] (NormalB bod) []
            where
              bod = AppE (AppE (VarE $ mkName "T.intercalate") (LitE (StringL "/")))
                       $ ListE (mkExp <$> cs)
              mkExp :: (String, CaptureOrPath) -> Exp
              mkExp (_, Path x) = LitE . StringL . T.unpack . T.toLower $ x
              mkExp (n, Capture) = AppE (VarE $ mkName "toUrlPiece") (VarE (mkName n))
              cs = over _1 pure <$> zip ['a'..'z'] xs
              newName | null name = mkName "Index"
                      | otherwise  = mkName name

instance (Typeable typ, HasRoute api, KnownSymbol name) =>
  HasRoute (Capture name typ :> api) where
    route Proxy routeInfo f = route (Proxy :: Proxy api) newRouteInfo f
      where
        newRouteInfo =
          routeInfo & routeADT . rADTFields %~ (flip (++)) [typ]
                    & toRouteInstance . capturesAndPaths . _2 %~ (flip snoc) Capture
        typ = ConT . mkName . show . typeRep $ (Proxy :: Proxy typ)

instance (KnownSymbol sym, HasRoute api) =>
  HasRoute (sym :> api) where
    route Proxy routeInfo f =
      route (Proxy :: Proxy api) newRouteInfo f
        where
          nam = capitalize $ symbolVal (Proxy :: Proxy sym)
          newRouteInfo =
            routeInfo & routeADT . rADTName %~ (++nam)
                      & toRouteInstance . capturesAndPaths . _1 %~ (flip (++)) nam
                      & toRouteInstance . capturesAndPaths . _2 %~ (flip snoc) (Path (T.pack nam))

instance (HasRoute l, HasRoute r) => HasRoute (l :<|> r) where
    route Proxy r f =
      route (Proxy :: Proxy l) r f <>
        route (Proxy :: Proxy r) r f

instance {-# overlappable #-}
  ( AllCTRender ctypes a, ReflectMethod method, KnownNat status
  ) => HasRoute (Verb method status ctypes a) where
    route Proxy r f = RouteResult ctors capOrPaths
      where
        ctors = [ NormalC nam typs ]
        typs = map (IsStrict,) (r ^. routeADT ^. rADTFields)
        nam | null (r ^. routeADT ^. rADTName) = mkName "Index"
            | otherwise = mkName $ capitalize $ f (r ^. routeADT ^. rADTName)
        capOrPaths = [ r ^. toRouteInstance ^. capturesAndPaths ]

instance {-# overlapping #-}
         ( AllCTRender ctypes a, ReflectMethod method, KnownNat status
         , GetHeaders (Headers h a)
         ) => HasRoute (Verb method status ctypes (Headers h a)) where
    route Proxy r f = RouteResult ctors capOrPaths
      where
        ctors = [ NormalC nam typs ]
        typs = map (IsStrict,) (r ^. routeADT ^. rADTFields)
        nam = mkName $ capitalize $ f (r ^. routeADT ^. rADTName)
        capOrPaths = [ r ^. toRouteInstance ^. capturesAndPaths ]

instance (Typeable typ, HasRoute api, KnownSymbol name) => HasRoute (QueryParam name typ :> api) where
    route _ = route (Proxy :: Proxy api) 
instance (Typeable typ, HasRoute api, KnownSymbol name) => HasRoute (QueryParams name typ :> api) where
    route _ = route (Proxy :: Proxy api) 
instance (HasRoute api, KnownSymbol name) => HasRoute (QueryFlag name :> api) where
    route _ = route (Proxy :: Proxy api) 
instance HasRoute api => HasRoute (HttpVersion :> api) where
    route _ = route (Proxy :: Proxy api) 
instance HasRoute api => HasRoute (Vault :> api) where
    route _ = route (Proxy :: Proxy api) 
instance HasRoute api => HasRoute (IsSecure :> api) where
    route _ = route (Proxy :: Proxy api) 
instance HasRoute api => HasRoute (RemoteHost :> api) where
    route _ = route (Proxy :: Proxy api) 
instance (Typeable typ, HasRoute api, KnownSymbol name) => HasRoute (Header name typ :> api) where
    route _ = route (Proxy :: Proxy api) 
instance ( AllCTUnrender list a, HasRoute api) => HasRoute (ReqBody list a :> api) where
    route _ = route (Proxy :: Proxy api) 

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

defOptions :: String -> String
defOptions = id

