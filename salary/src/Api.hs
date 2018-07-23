{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{- LANGUAGE TypeInType #-} -- FIXME type PeopleAPI = "people" :> Get '[JSON] [People] Не работает

module Api
    ( salaryApp
    ) where

import Servant.API
import Servant.Server

import Database.Beam
import Database.Beam.Postgres

import Data.Proxy
import Data.Aeson
import Data.Text (Text)

import Data

instance ToJSON (PeopleT Identity)

-- FIXME type PeopleAPI = "people" :> Get '[JSON] [People] Не работает
type PeopleAPI = "people" :> Get '[JSON] [PeopleT Identity]

type SalaryAPI = PeopleAPI

handlePeople = liftIO $ do
        conn <- connectPostgreSQL "postgresql://nlv@localhost/salary"
        runBeamPostgresDebug putStrLn conn $ do
            runSelectReturningList $ select allPeople
        where salaryDb :: DatabaseSettings be SalaryDb
              salaryDb = defaultDbSettings

              allPeople = all_ (_salaryPeople salaryDb)

salaryServer :: Server SalaryAPI
salaryServer = handlePeople

salaryAPI :: Proxy SalaryAPI
salaryAPI = Proxy

salaryApp :: Application
salaryApp = serve salaryAPI salaryServer

