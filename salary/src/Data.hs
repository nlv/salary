{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{- LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{- LANGUAGE PartialTypeSignatures #-}
{- LANGUAGE ImpredicativeTypes #-}
{- LANGUAGE NoMonomorphismRestriction #-}
{- LANGUAGE UndecidableInstances #-}

module Data
    ( PeopleT(..)
    , SalaryDb(..)
    ) where

import Database.Beam

import Data.Text (Text)


{-- People --}

data PeopleT f
    = People
    { _peopleId        :: Columnar f Int 
    , _peopleFirstName :: Columnar f Text
    , _peopleSurName   :: Columnar f Text
    , _peopleLastName  :: Columnar f Text 
    } deriving Generic

type People = PeopleT Identity
type PeopleId = PrimaryKey PeopleT Identity

deriving instance Show People
deriving instance Eq People

instance Beamable PeopleT

instance Table PeopleT where
    data PrimaryKey PeopleT f = PeopleId (Columnar f Int) deriving Generic
    primaryKey = PeopleId . _peopleId
instance Beamable (PrimaryKey PeopleT)

{-- Database --}

data SalaryDb f = SalaryDb
                      { _salaryPeople :: f (TableEntity PeopleT) }
                        deriving Generic

instance Database be SalaryDb

