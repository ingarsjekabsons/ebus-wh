{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API where

import Control.Exception (throw)
import Control.Monad.IO.Class (MonadIO (..))
import DB
  ( Products (..),
    createProduct,
    deleteProduct,
    getProduct,
    getProducts,
    updateProduct,
  )
import Data.Proxy (Proxy (..))
import Database.Esqueleto.Experimental
  ( ConnectionPool,
    Entity,
    SqlPersistT,
    entityKey,
    entityVal,
    fromSqlKey,
    runSqlPersistM,
    runSqlPersistMPool,
  )
import Deriving.Aeson.Stock (CustomJSON (CustomJSON), FromJSON, PrefixedSnake, ToJSON)
import GHC.Generics
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Servant (JSON, errBody)
import Servant.API (Capture, Delete, Get, Patch, Post, ReqBody, type (:<|>) ((:<|>)), type (:>))
import Servant.Server (Handler, Server, err404, err500, serve)

type ProductsAPI =
  "products" :> Get '[JSON] [Product]
    :<|> "products" :> Capture "productId" Integer :> Get '[JSON] Product
    :<|> "products" :> ReqBody '[JSON] Product :> Post '[JSON] Product
    :<|> "products" :> Capture "productId" Integer :> ReqBody '[JSON] Product :> Patch '[JSON] ()
    :<|> "products" :> Capture "productId" Integer :> Delete '[JSON] ()

productsAPI :: Proxy ProductsAPI
productsAPI = Proxy

server :: ConnectionPool -> Server ProductsAPI
server pool =
  apiGetProducts pool
    :<|> apiGetProduct pool
    :<|> apiCreateProduct pool
    :<|> apiUpdateProduct pool
    :<|> apiDeleteProduct pool

runAPI :: ConnectionPool -> Int -> IO ()
runAPI pool port = run port (simpleCors $ serve productsAPI $ server pool)

apiGetProducts :: ConnectionPool -> Handler [Product]
apiGetProducts p = liftIO $ (fmap . fmap) prodToDTO $ flip runSqlPersistMPool p $ do getProducts

apiGetProduct :: ConnectionPool -> Integer -> Handler Product
apiGetProduct p i = liftIO $
  flip runSqlPersistMPool p $ do
    prod <- getProduct i
    case prod of
      Nothing -> throw $ err404 {errBody = "Product not founc"}
      Just prod -> do return $ prodToDTO prod

apiCreateProduct :: ConnectionPool -> Product -> Handler Product
apiCreateProduct pool prod = liftIO $
  flip runSqlPersistMPool pool $ do
    res <- createProduct $ prodFromDTO prod
    case res of
      Nothing -> throw $ err500 {errBody = "System error"}
      Just p' -> return $ prodToDTO p'

apiUpdateProduct :: ConnectionPool -> Integer -> Product -> Handler ()
apiUpdateProduct pool pid prod = liftIO $
  flip runSqlPersistMPool pool $ do
    updateProduct
      (fromIntegral $ productId prod)
      (productName prod)
      (productDescription prod)
      (fromIntegral $ productInStock prod)
      (fromIntegral $ productPriceMinorUnits prod)

apiDeleteProduct :: ConnectionPool -> Integer -> Handler ()
apiDeleteProduct pool pid = liftIO $
  flip runSqlPersistMPool pool $ do
    deleteProduct (fromIntegral pid)

data Product = Product
  { productId :: Int,
    productName :: String,
    productDescription :: String,
    productInStock :: Int,
    productPriceMinorUnits :: Int
  }
  deriving stock (Ord, Eq, Generic)
  deriving (ToJSON) via PrefixedSnake "product" Product
  deriving (FromJSON) via PrefixedSnake "product" Product

prodToDTO :: Entity Products -> Product
prodToDTO p =
  let p' = entityVal p
   in Product
        (fromIntegral $ fromSqlKey $ entityKey p)
        (productsName p')
        (productsDescription p')
        (productsInStock p')
        (productsPriceMinorUnits  p')

prodFromDTO :: Product -> Products
prodFromDTO p =
  Products
    (productName p)
    (productDescription p)
    (productInStock p)
    (productPriceMinorUnits p)
