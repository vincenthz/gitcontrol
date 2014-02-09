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

commandParse :: ByteString -> Either String GitCommand
commandParse bs
    | gitReceivePackStr `B.isPrefixOf` bs = ReceivePack `fmapE` (getRepositoryPath $ B.drop (B.length gitReceivePackStr) bs)
    | gitUploadPackStr `B.isPrefixOf` bs  = UploadPack `fmapE` (getRepositoryPath $ B.drop (B.length gitUploadPackStr) bs)
    | otherwise                           = Left ("unknown command: " ++ show bs)
  where getRepositoryPath b
            | B.length b < 1      = Left "no path specified"
            | B.head b == quoteW8 = let (path, r) = getPathAndRem (B.drop 1 b)
                                     in case r of
                                            "\'" -> Right $ RepositoryPath path
                                            _    -> Left "bad path specification"
            | otherwise           =  let (path, r) = getPathAndRem b
                                      in case r of
                                            "" -> Right $ RepositoryPath path
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
