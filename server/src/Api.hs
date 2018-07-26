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
import Servant.Server.StaticFiles

import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Backend.SQL.BeamExtensions

import Data.Proxy
import Data.Aeson
import Data.Text (Text)

import Data

instance ToJSON (PeopleT Identity)
instance FromJSON (PeopleT Identity)
instance ToJSON (PrimaryKey PeopleT Identity)
instance FromJSON (PrimaryKey PeopleT Identity)
--instance FromHttpApiData (PrimaryKey PeopleT Identity)

-- FIXME type PeopleAPI = "people" :> Get '[JSON] [People] Не работает
type PeopleAPI = "people" :> 
                    (   Get '[JSON] [PeopleT Identity]
                    :<|> ReqBody '[JSON] (PeopleT Identity) :> Post '[JSON] (PrimaryKey PeopleT Identity)
                    -- :<|> Capture "peopleId" (PrimaryKey PeopleT Identity) :> ReqBody '[JSON] (PeopleT Identity) :> PutNoContent '[JSON] NoContent
                    :<|> Capture "peopleId" Int :> ReqBody '[JSON] (PeopleT Identity) :> PutNoContent '[JSON] NoContent
                    ) 

type StaticAPI = Raw

type SalaryAPI = PeopleAPI :<|> StaticAPI

salaryServer :: Server SalaryAPI
salaryServer = (getPeople :<|> postPeople :<|> putPeople) :<|> serveDirectoryWebApp "../reactjs-ui/web"

salaryAPI :: Proxy SalaryAPI
salaryAPI = Proxy

salaryApp :: Application
salaryApp = serve salaryAPI salaryServer

getPeople :: Handler [PeopleT Identity]
getPeople = liftIO $ do
        conn <- connectPostgreSQL "postgresql://nlv@localhost/salary"
        runBeamPostgresDebug putStrLn conn $ do
            runSelectReturningList $ select allPeople
        where salaryDb :: DatabaseSettings be SalaryDb
              salaryDb = defaultDbSettings

              allPeople = all_ (_salaryPeople salaryDb)

postPeople :: (PeopleT Identity) -> Handler PeopleId 
postPeople p0 = liftIO $ do
        conn <- connectPostgreSQL "postgresql://nlv@localhost/salary"
        [newPeople] <- runBeamPostgresDebug putStrLn conn $ do  
            runInsertReturningList (_salaryPeople salaryDb) $ 
              insertExpressions [People default_ (val_ $ _peopleFirstName p0) (val_ $ _peopleSurName p0) (val_ $ _peopleLastName p0)]
        return (pk newPeople)    
    where 
          salaryDb :: DatabaseSettings be SalaryDb
          salaryDb = defaultDbSettings

--putPeople :: (PrimaryKey PeopleT Identity) -> (PeopleT Identity) -> Handler NoContent
putPeople :: Int -> (PeopleT Identity) -> Handler NoContent
putPeople peopleId p0 = liftIO $ do
        conn <- connectPostgreSQL "postgresql://nlv@localhost/salary"
        runBeamPostgresDebug putStrLn conn $ do  
            runUpdate $ save (_salaryPeople salaryDb) newPeople
        return NoContent

        where newPeople = p0 { _peopleId = peopleId }
              salaryDb :: DatabaseSettings be SalaryDb
              salaryDb = defaultDbSettings
