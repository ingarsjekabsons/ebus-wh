{-# LANGUAGE OverloadedStrings #-}

module Main where

import API (runAPI)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT, runStdoutLoggingT)
import DB (Products (..), createProduct, getProducts, migrateAll)
import Database.Esqueleto.Experimental (runMigration)
import Database.Persist.Sqlite (createSqlitePool, runSqlite)

import System.Environment (getEnv)
main :: IO ()
main = do
  --runStderrLoggingT $ runSqlite "db.db" $ do
  --runMigration migrateAll
  -- _ <- createProduct $ Products "P1" "Tralala" 3
  -- _ <- createProduct $ Products "P2" "Tralala" 3
  -- _ <- createProduct $ Products "P3" "Tralala" 3
  -- _ <- createProduct $ Products "P4" "Tralala" 3
  --pure ()

  runSqlite "db.db" $ do
    runMigration migrateAll
  pool <- runStdoutLoggingT $ createSqlitePool "db.db" 4
  port <- getEnv "PORT"
  liftIO $ runAPI pool (read port)
