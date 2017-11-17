module DBInterface where

import Database.HDBC
import Database.HDBC.Sqlite3
import Data.List
import Data.Maybe
import Data.Time

joinByComma :: [String] -> String
joinByComma = intercalate ","
joinByAnd :: [String] -> String
joinByAnd = intercalate " and "
relate :: String -> String
relate x = x ++ "=?"
parenthesize :: String -> String
parenthesize x = "(" ++ x ++ ")"
makeCommaList :: [String] -> String
makeCommaList xs = joinByComma $ map relate xs
makeAndWheres :: [String] -> String
makeAndWheres xs = joinByAnd $ map relate xs
makeSelect :: String -> [String] -> String -> String
makeSelect table items criteria =
  "SELECT " ++ item_list ++ " FROM " ++ table ++ where_clause ++ ";"
  where
    item_list = if null items then "*" else joinByComma items
    where_clause = if null criteria then "" else " WHERE " ++ criteria
makeInsert :: String -> [String] -> String
makeInsert table items =
  "INSERT INTO " ++ table ++ item_list ++ " VALUES " ++ val_list ++ ";"
  where
    item_list = parenthesize $ joinByComma items
    val_list = parenthesize $ joinByComma $ replicate (length items) "?"
makeUpdate :: String -> [String] -> String -> String
makeUpdate table items criteria =
  "UPDATE " ++ table ++ " SET " ++ item_list ++ " WHERE " ++ where_clause ++ "?"
  where
    item_list = makeCommaList items
    where_clause = if null criteria then "" else " WHERE " ++ criteria
makeDelete :: String -> String -> String
makeDelete table criteria =
  "DELETE FROM " ++ table ++ where_clause
  where where_clause = if null criteria then "" else " WHERE " ++ criteria

selectX :: IConnection conn => conn -> String -> [String] -> String -> IO Statement
selectX conn table items criteria = prepare conn $ makeSelect table items criteria
deleteX :: IConnection conn => conn -> String -> String -> IO Statement
deleteX conn table criteria = prepare conn $ makeDelete table criteria
selectAllX :: IConnection conn => conn -> String -> IO [[(String, SqlValue)]]
selectAllX conn x = do
  sel_stmt <- selectX conn x [] []
  _ <- execute sel_stmt []
  fetchAllRowsAL sel_stmt

insertUsers :: IConnection conn => conn -> IO Statement
insertUsers conn = prepare conn $ makeInsert "users" ["username", "password"]

insertUser :: IConnection conn => conn -> String -> String -> IO ()
insertUser conn name pass = do
    ins_stmt <- insertUsers conn
    _ <- execute ins_stmt [toSql name, toSql pass]
    commit conn

updateUsers :: IConnection conn => conn -> [String] -> IO Statement
updateUsers conn items = prepare conn $ makeUpdate "users" items (relate "u_id")

deleteUsers :: IConnection conn => conn -> IO Statement
deleteUsers conn = deleteX conn "users" (relate "u_id")

selectAllUsers :: IConnection conn => conn -> IO [[(String, SqlValue)]]
selectAllUsers conn = selectAllX conn "users"

selectUsersById :: IConnection conn => conn -> IO Statement
selectUsersById conn = selectX conn "users" [] (relate "u_id")

selectPassByName :: IConnection conn => conn -> String -> IO String
selectPassByName conn name = do
  sel_stmt <- selectX conn "users" ["password"] (relate "username")
  _ <- execute sel_stmt [toSql (name :: String)]
  x <- fetchRow sel_stmt
  return $ if isJust x then fromSql (head (fromJust x)) :: String else ""

selectUsers :: IConnection conn => conn -> Int -> IO (Maybe [(String, SqlValue)])
selectUsers conn uid = do
  sel_stmt <- selectX conn "users" [] (relate "u_id")
  _ <- execute sel_stmt [toSql uid]
  fetchRowAL sel_stmt

--
selectUserByName' :: IConnection conn => conn -> String -> IO [[SqlValue]]
selectUserByName' conn name = quickQuery' conn "SELECT password FROM users WHERE username=?" [toSql name]
selectUserByName :: IConnection conn => conn -> String -> IO String
selectUserByName conn name = do
  x <- selectUserByName' conn name
  let y = if null x then [] else head x
  let a = if null y then "" else fromSql (head y) :: String
  return a

insertChat :: IConnection conn => conn -> IO Statement
insertChat conn = prepare conn $ makeInsert "chat" ["c_uid", "c_body"]

insertMsg :: IConnection conn => conn -> String -> String -> IO ()
insertMsg conn name msg = do
    let sel_stmt = makeSelect "users" ["u_id"] (relate "username")
    uid <- quickQuery' conn sel_stmt [toSql name]
    ins_stmt <- insertChat conn
    _ <- execute ins_stmt (head uid ++ [toSql msg])
    commit conn

selectMsgs :: IConnection conn => conn -> IO [(String, String, UTCTime)]
selectMsgs conn = do
    sel <- selectX conn "chat" ["c_username", "c_body", "c_post_time"] []
    _ <- execute sel []
    sql_tmp <- fetchAllRows sel
    return $ map (\(u:b:t:_) ->
        (fromSql u::String, fromSql b::String, fromSql t::UTCTime)) sql_tmp

updateChat :: IConnection conn => conn -> IO Statement
updateChat conn = prepare conn $ makeUpdate "chat" ["c_body"] (relate "c_id")

deleteChat :: IConnection conn => conn -> IO Statement
deleteChat conn = prepare conn $ makeInsert "hidden_messages" ["hm_uid", "hm_mid"]

undoDeleteChat :: IConnection conn => conn -> IO Statement
undoDeleteChat conn = deleteX conn "hidden_messages" criteria
    where criteria = makeAndWheres ["hm_uid", "hm_mid"]

selectAllChat :: IConnection conn => conn -> IO [[(String, SqlValue)]]
selectAllChat conn = selectAllX conn "chat"

selectUserChat :: IConnection conn => conn -> Int -> IO [[SqlValue]]
selectUserChat conn uid = quickQuery' conn "SELECT * FROM chat WHERE c_mid NOT IN \
                                    \(SELECT hm_mid FROM hidden_messages WHERE hm_uid=?)" [toSql uid]

cleanDb :: IConnection conn => conn -> IO ()
cleanDb conn = do
  del_hidden <- deleteX conn "hidden_messages" []
  del_messages <- deleteX conn "messages" []
  del_users <- deleteX conn "users" []
  _ <- execute del_hidden []
  _ <- execute del_messages []
  _ <- execute del_users []
  return ()

insertSampleData :: IConnection conn => conn -> IO ()
insertSampleData conn = do
  ins_users <- insertUsers conn
  ins_chat <- insertChat conn
  del_chat <- deleteChat conn
  executeMany ins_users [[toSql "aaa", toSql "pass1"]
                        ,[toSql "bbb", toSql "pass2"]
                        ,[toSql "ccc", toSql "pass3"]
                        ]
  executeMany ins_chat [[toSql (1 :: Int), toSql "aaa ccc vvv nnn"]
                       ,[toSql (1 :: Int), toSql "aaa ccc vvv nnn"]
                       ,[toSql (1 :: Int), toSql "aaa ccc vvv nnn"]
                       ,[toSql (2 :: Int), toSql "bbb ccc vvv nnn"]
                       ,[toSql (2 :: Int), toSql "bbb ccc vvv nnn"]
                       ,[toSql (2 :: Int), toSql "bbb ccc vvv nnn"]
                       ,[toSql (3 :: Int), toSql "ccc ccc vvv nnn"]
                       ,[toSql (3 :: Int), toSql "ccc ccc vvv nnn"]
                       ,[toSql (3 :: Int), toSql "ccc ccc vvv nnn"]
                       ]
  executeMany del_chat [[toSql (1 :: Int), toSql (4 :: Int)]
                       ,[toSql (1 :: Int), toSql (4 :: Int)]
                       ,[toSql (1 :: Int), toSql (7 :: Int)]
                       ,[toSql (3 :: Int), toSql (4 :: Int)]
                       ]

getSqlConnection :: FilePath -> IO Connection
getSqlConnection = connectSqlite3

{-
main = do
  conn <- connectSqlite3 db
  insert_user <- insertUsers conn
  --execute insert_user [toSql "aaa", toSql "zzz"]
  --commit conn
  select_users <- selectAllUsers conn
  x <- quickQuery' conn "SELECT * FROM users" []
  print x
  let y =  (head x)
  print (fromSql (head y) :: Integer)
  print (fromSql (y !! 1) :: String)
  print (fromSql (y !! 2) :: String)

  disconnect conn
-}
