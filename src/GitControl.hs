{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.IO.Class (liftIO)

import Data.ByteString.Char8 (ByteString)
import System.Posix.Env.ByteString
import System.Posix.Process.ByteString

import System.GitControl.Shell
import qualified System.GitControl.Class as DB
import qualified System.GitControl.Types as DB
import qualified System.GitControl.Dummy as DB

findIn :: ByteString -> [(ByteString, ByteString)] -> Maybe ByteString
findIn _   []         = Nothing
findIn key ((k,c):xs) = if (key == k) then Just c else findIn key xs

main = do
    args <- getArgs
    case args of
        [user] -> authUser user
        _      -> error "invalid command line"
  where authUser userName = do
            envs <- getEnvironment
            case findIn "SSH_ORIGINAL_COMMAND" envs of
                Nothing   -> Prelude.putStrLn "error, command not found"
                Just ocmd -> do let theCmd = commandLineParser ocmd
                                db <- liftIO $ DB.getPersistent :: IO [DB.Entity]
                                isAuth <- DB.hasRight db (DB.Username userName) (DB.RepositoryPath $ head $ gitCmdArgs theCmd) DB.AccessRead
                                case isAuth of
                                    False -> error $ "not authorized user: " ++ (show userName)
                                    True  -> executeFile (gitCmd theCmd) True (gitCmdArgs theCmd) (Just envs)
