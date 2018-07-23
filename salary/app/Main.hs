{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text)

import Network.Wai.Handler.Warp

import Data
import Api

import Database.Beam
import Database.Beam.Postgres


main :: IO ()
main = someFunc2

someFunc2 :: IO ()
someFunc2 = run 8081 salaryApp

someFunc :: IO ()
someFunc = do
        conn <- connectPostgreSQL "postgresql://nlv@localhost/salary"
        runBeamPostgresDebug putStrLn conn $ do
            people <- runSelectReturningList $ select allPeople
            mapM_ (liftIO . putStrLn . show) people
        where salaryDb :: DatabaseSettings be SalaryDb
              salaryDb = defaultDbSettings

              allPeople = all_ (_salaryPeople salaryDb)
