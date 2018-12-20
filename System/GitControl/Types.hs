module System.GitControl.Types where

import Data.ByteString (ByteString)

data AccessMode = None | AccessRead | AccessWrite
    deriving (Show, Read, Eq)
instance Ord AccessMode where
    compare None        None        = EQ
    compare None        AccessRead  = LT
    compare None        AccessWrite = LT
    compare AccessRead  None        = GT
    compare AccessRead  AccessRead  = EQ
    compare AccessRead  AccessWrite = LT
    compare AccessWrite None        = GT
    compare AccessWrite AccessRead  = GT
    compare AccessWrite AccessWrite = EQ

newtype Username = Username ByteString
    deriving (Show,Read,Eq,Ord)

newtype RepositoryPath = RepositoryPath { getRepositoryPathAsBytes :: ByteString }
    deriving (Show,Read,Eq,Ord)
