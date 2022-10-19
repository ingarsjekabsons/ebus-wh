{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module DB
  ( getProducts,
    getProduct,
    migrateAll,
    createProduct,
    updateProduct,
    deleteProduct,
    Products (..),
  )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (logDebugN)
import Control.Monad.Logger.CallStack (MonadLogger)
import Control.Monad.Reader (ReaderT)
import Data.Aeson (FromJSON)
import Data.Aeson.Types (ToJSON)
import Database.Esqueleto.Experimental
  ( Entity (entityVal),
    SqlBackend,
    SqlPersistT,
    delete,
    from,
    insertUniqueEntity,
    select,
    selectOne,
    set,
    update,
    upsert,
    val,
    valkey,
    where_,
    (=.),
    (==.),
    (^.),
  )
import Database.Esqueleto.Experimental.From (table)
import Database.Persist.TH
  ( mkMigrate,
    mkPersist,
    persistLowerCase,
    share,
    sqlSettings,
  )
import GHC.Generics (Generic)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
    Products
      name String
      description String
      inStock Int
      UniqueProduct name 
      deriving Eq Show
  |]

getProducts :: (MonadIO m, MonadLogger m) => SqlPersistT m [Entity Products]
getProducts = do
  select $ from $ table @Products

getProduct :: (MonadIO m, MonadLogger m) => Integer -> SqlPersistT m (Maybe (Entity Products))
getProduct pid = do
  selectOne $ do
    prod <- from $ table @Products
    where_ (prod ^. ProductsId ==. valkey (fromIntegral pid))
    return prod

deleteProduct :: (MonadIO m, MonadLogger m) => Int -> SqlPersistT m ()
deleteProduct pid = do
  delete $ do
    prod <- from $ table @Products
    where_ (prod ^. ProductsId ==. valkey (fromIntegral pid))

updateProduct :: (MonadIO m, MonadLogger m) => Integer -> String -> String -> Integer -> SqlPersistT m ()
updateProduct pid name descr count = do
  update $ \p -> do
    set p [ProductsName =. val name, ProductsDescription =. val descr, ProductsInStock =. val (fromIntegral count)]
    where_ (p ^. ProductsId ==. valkey (fromIntegral pid))

createProduct :: (MonadIO m, MonadLogger m) => Products -> SqlPersistT m (Maybe (Entity Products))
createProduct prod = do
  insertUniqueEntity prod