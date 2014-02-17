{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Control.Applicative
import qualified Data.ByteString.Char8 as BS
import System.Posix.Env.ByteString

import System.GitControl

import Data.List
import qualified Data.Map as M

-- format of the file:
--
-- user:repo:access
--
-- where access = 'r' (read-only) or 'w' (read-write)

getPath :: IO BS.ByteString
getPath = maybe (error "no HOME defined") (flip BS.append "/gitcontrol") `fmap` getEnv "HOME"

getDb :: IO Db
getDb = do
    path <- BS.unpack <$> getPath
    Db . foldl' doAcc M.empty . BS.lines <$> BS.readFile path
  where doAcc acc bs
            | BS.length bs < 5       = acc
            | "#" `BS.isPrefixOf` bs = acc
            | otherwise =
                case BS.split ':' bs of
                    [user,repo,"r"] -> add acc AccessRead user repo
                    [user,repo,"w"] -> add acc AccessWrite user repo
                    _               -> acc
        add acc access user repo
            | BS.length user > 0 && BS.length repo > 0 = M.insert (Username user, RepositoryPath repo) access acc
            | otherwise                                = acc

newtype Db = Db (M.Map (Username, RepositoryPath) AccessMode)

instance GitControl Db where
    isAuthorized _      _     _     None  = return False
    isAuthorized (Db m) uName rName aMode =
        case M.lookup (uName,rName) m of
            Nothing    -> return False
            Just right -> return $ aMode <= right

main :: IO ()
main = (maybe (error "no HOME defined") (flip BS.append "/")) `fmap` getEnv "HOME"
   >>= \h -> defaultMain h getDb
