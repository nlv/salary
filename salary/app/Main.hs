{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text)
import Database.Beam
import Database.Beam.Postgres
import Data

main :: IO ()
main = someFunc

someFunc :: IO ()
someFunc = do
        conn <- connectPostgreSQL "postgresql://nlv@localhost/salary"
        runBeamPostgresDebug putStrLn conn $ do
            people <- runSelectReturningList $ select allPeople
            mapM_ (liftIO . putStrLn . show) people
        where salaryDb :: DatabaseSettings be SalaryDb
              salaryDb = defaultDbSettings

              allPeople = all_ (_salaryPeople salaryDb)
