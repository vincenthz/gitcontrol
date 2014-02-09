{-# LANGUAGE OverloadedStrings #-}
module System.GitControl.Shell
    ( -- * Command line parser
      GitCommand(..)
    , commandParse
    , commandToAccess
    , commandToRaw
    ) where

import System.GitControl.Types
import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()
import qualified Data.ByteString as B

data GitCommand = ReceivePack { getCommandRepository :: RepositoryPath }
                | UploadPack  { getCommandRepository :: RepositoryPath }
                deriving (Eq)

gitReceivePackStr, gitUploadPackStr :: ByteString
gitReceivePackStr = "git-receive-pack "
gitUploadPackStr  = "git-upload-pack "

data HasDDot = NoDDot
             | MaybeDDot
             | HasDDot
             deriving (Eq)

-- | create a new repository path with the following contraints:
--
-- * length between 1 and 1024
-- * not starting by '/' or '.' or '~'
-- * only 1 '/' separator
-- * no ".."
-- * no unicode character
repositoryPath :: ByteString -> Either String RepositoryPath
repositoryPath path
    | B.length path == 0   = Left "empty path"
    | B.length path > 1024 = Left "big path"
    | B.head path == tilW8 = Left "start by tilde"
    | B.head path == sepW8 = Left "start by separator"
    | B.head path == dotW8 = Left "start by dot"
    | B.last path == sepW8 = Left "end by separator"
    | numberOfSeps /= 1    = Left "0 or more than 1 separators"
    | numberOfUnicodes > 0 = Left "unicode character"
    | containsParent       = Left "parent in repository path"
    | otherwise            = Right $ RepositoryPath path
  where dotW8 = fromIntegral $ fromEnum '.'
        sepW8 = fromIntegral $ fromEnum '/'
        tilW8 = fromIntegral $ fromEnum '~'
        numberOfUnicodes = B.foldl' (\acc w -> acc + (if w > 0x7f then 1 else 0)) 0 path :: Int
        numberOfSeps   = B.foldl' (\acc w -> acc + (if w == sepW8 then 1 else 0)) 0 path :: Int
        containsParent = B.foldl' acc NoDDot path == HasDDot
          where acc HasDDot _ = HasDDot
                acc MaybeDDot v | v == dotW8 = HasDDot
                                | otherwise  = NoDDot
                acc NoDDot v | v == dotW8 = MaybeDDot
                             | otherwise  = NoDDot

commandParse :: ByteString -> Either String GitCommand
commandParse bs
    | gitReceivePackStr `B.isPrefixOf` bs = ReceivePack `fmapE` (getRepositoryPath $ B.drop (B.length gitReceivePackStr) bs)
    | gitUploadPackStr `B.isPrefixOf` bs  = UploadPack `fmapE` (getRepositoryPath $ B.drop (B.length gitUploadPackStr) bs)
    | otherwise                           = Left ("unknown command: " ++ show bs)
  where getRepositoryPath b
            | B.length b < 1      = Left "no path specified"
            | B.head b == quoteW8 = let (path, r) = getPathAndRem (B.drop 1 b)
                                     in case r of
                                            "\'" -> repositoryPath path
                                            _    -> Left "bad path specification"
            | otherwise           =  let (path, r) = getPathAndRem b
                                      in case r of
                                            "" -> repositoryPath path
                                            _  -> Left "bad path specification"
        getPathAndRem = B.break (\c -> c <= 0x20 || c == quoteW8)
        quoteW8 = fromIntegral $ fromEnum '\''

        -- for compat with ghc < 7.6
        fmapE _ (Left z)  = Left z
        fmapE f (Right z) = Right (f z)

commandToAccess :: GitCommand -> AccessMode
commandToAccess (ReceivePack _) = AccessRead
commandToAccess (UploadPack _)  = AccessWrite

commandToRaw :: GitCommand -> ByteString
commandToRaw (ReceivePack _) = gitReceivePackStr
commandToRaw (UploadPack _)  = gitUploadPackStr
