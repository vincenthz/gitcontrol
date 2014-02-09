module System.GitControl.Types where

import Data.ByteString (ByteString)

data AccessMode = AccessRead | AccessWrite
    deriving (Show, Read, Eq)

newtype Username = Username ByteString
    deriving (Show,Read,Eq)

newtype RepositoryPath = RepositoryPath ByteString
    deriving (Show,Read,Eq)
