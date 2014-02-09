{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module System.GitControl
    where

import System.GitControl.Class
import Data.List
import qualified Data.ByteString.Char8 as BS
import System.Posix.Env.ByteString
import System.Posix.Files.ByteString

data Entity = Entity Int BS.ByteString
    deriving (Show,Read)
instance Eq Entity where
    (Entity _ v1) == (Entity _ v2) = v1 == v2

getSerializedPath :: IO (Maybe BS.ByteString)
getSerializedPath = do home <- getEnv "HOME"
                       return $ case home of
                           Nothing -> Nothing
                           Just h  -> Just $ BS.concat [h,"/.git.control"]

instance GitControl [Entity] Entity where
    init        = do serializedFilePath <- getSerializedPath
                     case serializedFilePath of
                         Nothing   -> return []
                         Just path -> do
                             exist <- fileExist path
                             if exist then do serialized <- readFile $ BS.unpack path
                                              return $ read serialized
                                      else return []
    save     xs = do serializedFilePath <- getSerializedPath
                     case serializedFilePath of
                         Nothing   -> return ()
                         Just path -> writeFile (BS.unpack path) $ show xs
    insert a xs = if (a `elem` xs) then Left xs else Right (a:xs)
    member f xs = find f xs
    getBy  k xs = find (\(Entity k1 _) -> k == k1) xs
