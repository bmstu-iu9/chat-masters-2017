import Database.HDBC
import Database.HDBC.Sqlite3
import Data.List

db = "db.db"

prepareUpdates [] = []
prepareUpdates xs = map (\x -> show (fst x) ++ "=" ++ show (snd x)) xs
makeUpdates xs = intercalate "," (prepareUpdates xs)

insertUsers :: IConnection conn => conn -> IO Statement
insertUsers conn = prepare conn "INSERT INTO users(username,password) VALUES (?,?)"
updateUsers :: (Show a, Show a1, IConnection conn) => conn -> [(a1, a)] -> IO Statement
updateUsers conn upd = prepare conn $ "UPDATE users SET " ++ makeUpdates upd ++ "WHERE u_id=?"
deleteUsers :: IConnection conn => conn -> IO Statement
deleteUsers conn = prepare conn "DELETE FROM users WHERE u_id=?"
selectAllUsers :: IConnection conn => conn -> IO [[SqlValue]]
selectAllUsers conn = quickQuery' conn "SELECT * FROM users" []
selectUsers :: IConnection conn => conn -> Int -> IO [[SqlValue]]
selectUsers conn uid = quickQuery' conn "SELECT * FROM users WHERE u_id=?" [toSql uid]

insertChat :: IConnection conn => conn -> IO Statement
insertChat conn = prepare conn "INSERT INTO chat(c_uid,c_body) VALUES (?,?)"
updateChat :: IConnection conn => conn -> IO Statement
updateChat conn = prepare conn "UPDATE chat SET c_body=? WHERE c_id=?"
deleteChat :: IConnection conn => conn -> IO Statement
deleteChat conn = prepare conn "INSERT INTO hidden_message(hm_uid,hm_mid) VALUES (?,?)"
undoDeleteChat :: IConnection conn => conn -> IO Statement
undoDeleteChat conn = prepare conn "DELETE FROM hidden_messages WHERE hm_uid=? AND hm_mid=?"
selectAllChat :: IConnection conn => conn -> IO [[SqlValue]]
selectAllChat conn = quickQuery' conn "SELECT * FROM chat" []
selectUserChat :: IConnection conn => conn -> Int -> IO [[SqlValue]]
selectUserChat conn uid = quickQuery' conn "SELECT * FROM chat WHERE c_mid NOT IN \
                                    \(SELECT hm_mid FROM hidden_messages WHERE hm_uid=?)" [toSql uid]

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
