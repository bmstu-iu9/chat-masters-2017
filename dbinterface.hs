module DBInterface where

import Database.HDBC
import Database.HDBC.Sqlite3
import Data.List

db :: String
db = "db.db"

joinByComma :: [String] -> String
joinByComma = intercalate ","
joinByAnd :: [String] -> String
joinByAnd = intercalate " and "
--joinByOr :: [String] -> String
--joinByOr = intercalate " or "
relate :: String -> String
relate x = x ++ "=?"
parenthesize :: String -> String
parenthesize x = "(" ++ x ++ ")"

makeCommaList :: [String] -> String
makeCommaList xs = joinByComma $ map relate xs
makeAndWheres :: [String] -> String
makeAndWheres xs = joinByAnd $ map relate xs
--makeOrWheres :: [String] -> String
--makeOrWheres xs = joinByOr $ map relate xs

makeSelect :: String -> [String] -> String -> String
makeSelect table items criteria =
  "SELECT " ++ item_list ++ " FROM " ++ table ++ where_clause ++ ";"
  where
    item_list = if null items then "*" else joinByComma items
    where_clause = if null criteria then "" else " WHERE " ++ criteria

select :: IConnection conn => conn -> String -> [String] -> String -> IO Statement
select conn table items criteria = prepare conn $ makeSelect table items criteria
selectAllUsers :: IConnection conn => conn -> IO Statement
selectAllUsers conn = select conn "users" [] []
selectUsersById :: IConnection conn => conn -> IO Statement
selectUsersById conn = select conn "users" [] (relate "u_id")
selectPassByName :: IConnection conn => conn -> IO Statement
selectPassByName conn = select conn "users" ["password"] (relate "username")
selectAllChat :: IConnection conn => conn -> IO Statement
selectAllChat conn = select conn "chat" [] []
selectUserChat :: IConnection conn => conn -> IO Statement
selectUserChat conn = do
  let banned = makeSelect "hidden_messages" ["hm_mid"] (relate "hm_uid")
  select conn "chat" [] ("c_mid NOT IN " ++ banned)

makeInsert :: String -> [String] -> String
makeInsert table items =
  "INSERT INTO " ++ table ++ item_list ++ " VALUES " ++ val_list ++ ";"
  where
    item_list = parenthesize $ joinByComma items
    val_list = parenthesize $ joinByComma $ replicate (length items) "?"

insertUsers :: IConnection conn => conn -> IO Statement
insertUsers conn = prepare conn $ makeInsert "users" ["username", "password"]
insertChat :: IConnection conn => conn -> IO Statement
insertChat conn = prepare conn $ makeInsert "chat" ["c_uid", "c_body"]
deleteChat :: IConnection conn => conn -> IO Statement
deleteChat conn = prepare conn $ makeInsert "hidden_messages" ["hm_uid", "hm_mid"]

makeUpdate :: String -> [String] -> String -> String
makeUpdate table items criteria =
  "UPDATE " ++ table ++ " SET " ++ item_list ++ " WHERE " ++ where_clause ++ "?"
  where
    item_list = makeCommaList items
    where_clause = if null criteria then "" else " WHERE " ++ criteria

updateChat :: IConnection conn => conn -> IO Statement
updateChat conn = prepare conn $ makeUpdate "chat" ["c_body"] (relate "c_id")

makeDelete :: String -> String -> String
makeDelete table criteria =
  "DELETE FROM " ++ table ++ where_clause
  where where_clause = if null criteria then "" else " WHERE " ++ criteria
deleteX :: IConnection conn => conn -> String -> String -> IO Statement
deleteX conn table criteria = prepare conn $ makeDelete table criteria

deleteUsers :: IConnection conn => conn -> IO Statement
deleteUsers conn = deleteX conn "users" (relate "u_id")
undoDeleteChat :: IConnection conn => conn -> IO Statement
undoDeleteChat conn = deleteX conn "hidden_messages" criteria
    where criteria = makeAndWheres ["hm_uid", "hm_mid"]

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
{-example_select conn = do
  q <- select conn "users" ["username", "password"] ["u_id"]
  execute q [toSql (1 :: Integer)]
  x1 <- fetchRowAL q -- Nothing | Just
  print x1
  execute q [toSql (1 :: Integer)]
  x2 <- fetchRowMap q -- Nothing | Just
  print x2
  execute q [toSql (1 :: Integer)]
  x3 <- fetchAllRowsAL q -- []
  print x3
  execute q [toSql (1 :: Integer)]
  x4 <- fetchAllRowsMap q -- []
  print x4-}

{-
main :: IO ()
main = do
  conn <- connectSqlite3 db
  cleanDb conn
  commit conn
  insertSampleData conn
  commit conn
  select_users <- selectAllUsers conn
  x <- quickQuery' conn "SELECT * FROM users" []
  print x
  let y =  head x
  print (fromSql (head y) :: Integer)
  print (fromSql (y !! 1) :: String)
  print (fromSql (y !! 2) :: String)

  disconnect conn
-}
