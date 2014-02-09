{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Data.List
import qualified Data.ByteString.Char8 as BS
import System.Posix.Env.ByteString
import System.Posix.Files.ByteString

import System.GitControl

data Entity = Entity
    { userName :: Username
    , repoName :: RepositoryPath
    , priv     :: AccessMode
    } deriving (Show,Read,Eq)

getSerializedPath :: IO (Maybe BS.ByteString)
getSerializedPath = do home <- getEnv "HOME"
                       return $ case home of
                           Nothing -> Nothing
                           Just h  -> Just $ BS.concat [h,"/.git.control"]

getPersistent :: IO [Entity]
getPersistent = do
    serializedFilePath <- getSerializedPath
    case serializedFilePath of
        Nothing   -> return []
        Just path -> do
            exist <- fileExist path
            if exist then do serialized <- readFile $ BS.unpack path
                             return $ read serialized
                     else return []
setPersistent xs = do
    serializedFilePath <- getSerializedPath
    case serializedFilePath of
        Nothing   -> return ()
        Just path -> writeFile (BS.unpack path) $ show xs

instance GitControl [Entity] where
    isAuthorized _  _     _     None  = return False
    isAuthorized xs uName rName aMode =
        let e = find (\t -> ((userName t) == uName) && ((repoName t) == rName)) xs
        in case e of
            Nothing                 -> return False
            Just (Entity _ _ None)  -> return False
            Just (Entity _ _ right) -> return $ aMode <= right

main = defaultMain getPersistent
