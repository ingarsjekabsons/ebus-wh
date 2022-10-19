{-# LANGUAGE OverloadedStrings #-}
module Main where

import Database.Esqueleto.Experimental (runMigration)
import Database.Persist.Sqlite (runSqlite, createSqlitePool)
import Control.Monad.IO.Class (liftIO)

import DB(getProducts, migrateAll, createProduct, Products(..))
import API(runAPI)
import Control.Monad.Logger (runStderrLoggingT, runStdoutLoggingT)

main :: IO ()
main = do
    --runStderrLoggingT $ runSqlite "db.db" $ do
        --runMigration migrateAll
        -- _ <- createProduct $ Products "P1" "Tralala" 3
        -- _ <- createProduct $ Products "P2" "Tralala" 3
        -- _ <- createProduct $ Products "P3" "Tralala" 3
        -- _ <- createProduct $ Products "P4" "Tralala" 3
        --pure ()

        pool <- runStdoutLoggingT $ createSqlitePool "db.db" 4
        liftIO $ runAPI pool 8888
        




