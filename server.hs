{-# LANGUAGE OverloadedStrings #-}
module Main where
import DBInterface

import Text.Printf
import Data.String
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Network.WebSockets as WS

type Client = (Text, WS.Connection)

type ServerState = [Client]

db = "db.db"

newServerState :: ServerState
newServerState = []

numClients :: ServerState -> Int
numClients = length

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
     T.putStrLn message
     forM_ clients $ \(_, conn) -> WS.sendTextData conn message

main :: IO ()
main = do
     dbConn <- getSqlConnection db
     state <- newMVar newServerState
     WS.runServer "127.0.0.1" 9160 $ application dbConn state

--application :: MVar ServerState -> WS.ServerApp
application dbConn state pending = do
     conn <- WS.acceptRequest pending
     WS.forkPingThread conn 30

     msg <- WS.receiveData conn
     clients <- readMVar state
     case msg of

         _   | not (prefix `T.isPrefixOf` msg) ->
                 WS.sendTextData conn ("Wrong announcement" :: Text)

             | any ($ fst client)
                 [T.null, T.any isPunctuation, T.any isSpace] ->
                     WS.sendTextData conn ("Name cannot " `mappend`
                         "contain punctuation or whitespace, and " `mappend`
                         "cannot be empty" :: Text)

             | clientExists client clients ->
                 WS.sendTextData conn ("User already exists" :: Text)

             | otherwise -> flip finally disconnect $ do

                modifyMVar_ state $ \s -> do
                    let s' = addClient client s
                    WS.sendTextData conn $
                        "Welcome! Users: " `mappend`
                        T.intercalate ", " (map fst s)

                    msgs <- selectMsgs dbConn
                    loadHistory conn msgs

                    broadcast (fst client `mappend` " joined") s'
                    return s'
                talk conn dbConn state client
           where
             prefix     = "Hi! I am "
             client     = (T.drop (T.length prefix) msg, conn)
             disconnect = do
                 -- Remove client and return new state
                 s <- modifyMVar state $ \s ->
                     let s' = removeClient client s in return (s', s')
                 broadcast (fst client `mappend` " disconnected") s


--talk :: WS.Connection -> MVar ServerState -> Client -> IO ()
talk conn dbConn state (user, u_ws) = do
    forever $ do
     msg <- WS.receiveData conn
     insertMsg dbConn (T.unpack user) (T.unpack msg)
     readMVar state >>= broadcast
         (user `mappend` ": " `mappend` msg)


loadHistory conn [] = do
    WS.sendTextData conn ("________________" :: Text)
loadHistory conn lst = do
    let msg@(user, body, time) = head lst
    WS.sendTextData conn ((fromString(user ++ ": " ++ body))::Text)
    loadHistory conn (tail lst)

