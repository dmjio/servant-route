servant-route
==================
Type-safe web routes generated from a servant DSL

*Coming soon*
 - Route parser

```haskell
type API = 
  "home" :> QueryParam "heh" Text :> Post '[JSON] [Int] :<|>
  "about":> Capture "foo" Int :> Get '[JSON] [Int]

-- data Route = Home !Text | About !Int, generated from TH

$(getRoute (Proxy :: Proxy API) id)
```
