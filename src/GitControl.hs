{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)

import Data.ByteString.Char8
import System.Posix.Env.ByteString
import System.Posix.Process.ByteString

import System.GitControl.Shell
import qualified System.GitControl.Class as DB
import qualified System.GitControl as DB

findIn :: ByteString -> [(ByteString, ByteString)] -> Maybe ByteString
findIn _   []         = Nothing
findIn key ((k,c):xs) = if (key == k) then Just c else findIn key xs

main = do
    args <- getArgs
    when (1 /= (Prelude.length args)) (error "invalid command line")
    let userName = Prelude.head args
    envs <- getEnvironment
    case findIn "SSH_ORIGINAL_COMMAND" envs of
        Nothing   -> Prelude.putStrLn "error, command not found"
        Just ocmd -> do let theCmd = commandLineParser ocmd
                        db <- liftIO $ DB.init :: IO [DB.Entity]
                        case DB.member (\(DB.Entity _ n) -> n == userName) db of
                            Nothing -> error $ "not authorized user: " ++ (unpack userName)
                            Just _  -> executeFile (gitCmd theCmd) True (gitCmdArgs theCmd) (Just envs)
