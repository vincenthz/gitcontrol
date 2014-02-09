module System.GitControl.SQLite
    where

import System.GitControl.Types
import System.GitControl.Class
import Database.HDBC
import Database.HDBC.Sqlite3

connectToSqlite = connectSqlite3 "test.db"

disconnectFromSqlite conn = disconnect conn

createSQLiteBase conn = do
    run conn
        "CREATE TABLE user (id INTEGER PRIMARY KEY, user BLOB NOT NULL, repo BLOB NOT NULL, access INTEGER NOT NULL)"
        []
    commit conn

instance GitControl Connection where
    isAuthorized _  _     _     None  = return False
    isAuthorized db (Username uName) (RepositoryPath rName) aMode = do
        list <- quickQuery' db "SELECT access FROM user WHERE user.user = ? AND user.repo = ?" [toSql $ uName, toSql rName]
        putStrLn $ show list
        return False
