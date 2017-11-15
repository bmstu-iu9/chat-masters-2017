module DBInterface where

import Database.HDBC
import Database.HDBC.Sqlite3
import Data.List

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

insertUsers :: IConnection conn => conn -> IO Statement
insertUsers conn = prepare conn $ makeInsert "users" ["username", "password"]

updateUsers :: IConnection conn => conn -> [String] -> IO Statement
updateUsers conn items = prepare conn $ makeUpdate "users" items (relate "u_id")

deleteUsers :: IConnection conn => conn -> IO Statement
deleteUsers conn = deleteX conn "users" (relate "u_id")

selectAllUsers :: IConnection conn => conn -> IO [[SqlValue]]
selectAllUsers conn = quickQuery' conn "SELECT * FROM users" []

selectAllUsers_stmt :: IConnection conn => conn -> IO Statement
selectAllUsers_stmt conn = selectX conn "users" [] []

selectUsersById :: IConnection conn => conn -> IO Statement
selectUsersById conn = selectX conn "users" [] (relate "u_id")

selectPassByName :: IConnection conn => conn -> IO Statement
selectPassByName conn = selectX conn "users" ["password"] (relate "username")

selectUsers :: IConnection conn => conn -> Int -> IO [[SqlValue]]
selectUsers conn uid = quickQuery' conn "SELECT * FROM users WHERE u_id=?" [toSql uid]

--
selectUserByName' :: IConnection conn => conn -> String -> IO [[SqlValue]]
selectUserByName' conn name = quickQuery' conn "SELECT password FROM users WHERE username=?" [toSql name]
selectUserByName conn name = do
  x <- selectUserByName' conn name
  let y = if (null x) then [] else head x
  let a = if (null y) then "" else (fromSql (head y)) :: [Char]
  return a

insertChat :: IConnection conn => conn -> IO Statement
insertChat conn = prepare conn $ makeInsert "chat" ["c_uid", "c_body"]

updateChat :: IConnection conn => conn -> IO Statement
updateChat conn = prepare conn $ makeUpdate "chat" ["c_body"] (relate "c_id")

deleteChat :: IConnection conn => conn -> IO Statement
deleteChat conn = prepare conn "INSERT INTO hidden_message(hm_uid,hm_mid) VALUES (?,?)"

undoDeleteChat :: IConnection conn => conn -> IO Statement
undoDeleteChat conn = deleteX conn "hidden_messages" criteria
    where criteria = makeAndWheres ["hm_uid", "hm_mid"]

selectAllChat :: IConnection conn => conn -> IO [[(String, SqlValue)]]
selectAllChat conn = do
  sel_stmt <- selectX conn "users" [] []
  _ <- execute sel_stmt []
  fetchAllRowsAL sel_stmt

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

getSqlConnection db = connectSqlite3 db

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
