{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{- LANGUAGE TypeInType #-}

module Api
    ( salaryApp
    ) where

import Servant.API
import Servant.Server

-- FIXME type PeopleAPI = "people" :> Get '[JSON] [People] Не работает
import Database.Beam

import Data.Proxy
import Data.Aeson

import Data


instance ToJSON (PeopleT Identity)

-- FIXME type PeopleAPI = "people" :> Get '[JSON] [People] Не работает
type PeopleAPI = "people" :> Get '[JSON] [PeopleT Identity]

type SalaryAPI = PeopleAPI

handlePeople = return []

salaryServer :: Server SalaryAPI
salaryServer = handlePeople

salaryAPI :: Proxy SalaryAPI
salaryAPI = Proxy

salaryApp :: Application
salaryApp = serve salaryAPI salaryServer

