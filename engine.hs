import DBInterface

import Control.Monad
import Data.Char
import System.IO
import Network
import Data.Time.LocalTime
import Data.Maybe
import Data.Char
import Debug.Trace
import System.Directory
import Text.Printf
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

db = "db.db"

redirects = [("/","/main")]

data RequestType = GET | POST deriving (Show)
data Request = Request { rtype :: RequestType, path :: String, args :: [(String, String)], options :: [(String,String)] }
data Response = Response { version :: String, statuscode :: Int, headers :: [String] }

instance Show Request where
  show r = "Request { " ++ show((rtype r)) ++ " " ++ (path r) ++ "\nArguments here:\n" ++ show (args r) ++ "\nOptions here:\n" ++ show (options r) ++ "\n}"

instance Show Response where
  show r = version(r) ++ " " ++ show(statuscode(r)) ++ " " ++ (case statuscode(r) of
    100 -> "Continue"
    200 -> "OK"
    301 -> "Moved Permanently"
    303 -> "See Other"
    404 -> "Not Found") ++ "\r\n" ++ (foldl (\acc str -> acc ++ str ++ "\r\n") "" (headers r)) ++ "\r\n"

fromString :: String -> RequestType
fromString t = case t of
  "GET" -> GET
  "POST" -> POST 

mainPage request = do
  html <- readFile "client.html" 
  let answer = if user_id /= Nothing
                then show(Response {version = "HTTP/1.1", statuscode = 200, headers=[]}) ++ (printf html (fromJust user_id))
                else show(Response {version = "HTTP/1.1", statuscode = 303, headers=["Location: /login"]})
  return answer where
    cookies = lookup "Cookie" (options(request))
    cookiesList = if cookies /= Nothing
                    then parseCookie (fromJust cookies)
                    else []
    user_id = lookup "user_id" cookiesList
  

registerPage request = do
  let answer = if ((login /= Nothing) && (pass /= Nothing))
                then do
                  conn <- getSqlConnection db  
                  insertUser conn (fromJust login) (fromJust pass)
                  sqlDisconnect conn
                  return $ show(Response {version = "HTTP/1.1", statuscode = 303, headers=["Set-Cookie: user_id=" ++ fromJust login, "Location: /main"]})
                else do 
                  html <- readFile "register.html" 
                  return $ show(Response {version = "HTTP/1.1", statuscode = 200, headers=[]}) ++ html
  answer where
    login = lookup "loginField" (args(request))
    pass = lookup "passwordField" (args(request))
    
loginPage request = do
  html <- readFile "login.html" 
  return $ show(Response {version = "HTTP/1.1", statuscode = 200, headers=[]}) ++ html
    

processAjax request = do
  let login = lookup "login" (args (request))
  if login /= Nothing
    then do
      conn <- getSqlConnection db  
      db_pass <- selectUserByName conn (fromJust login)
      sqlDisconnect conn
      let pass = lookup "password" (args request)
      if pass == Nothing
        then if (null db_pass)
              then return $ show(Response {version = "HTTP/1.1", statuscode = 200, headers=["Access-Control-Allow-Origin: *"]})
              else return $ show(Response {version = "HTTP/1.1", statuscode = 404, headers=["Access-Control-Allow-Origin: *"]})
        else if (null db_pass) || (db_pass /= (fromJust pass))
              then return $ show(Response {version = "HTTP/1.1", statuscode = 404, headers=["Access-Control-Allow-Origin: *"]})
              else return $ show(Response {version = "HTTP/1.1", statuscode = 200, headers=["Access-Control-Allow-Origin: *"]})
    else return $ show(Response {version = "HTTP/1.1", statuscode = 404, headers=["Access-Control-Allow-Origin: *"]})
        

errorPage request = return answer
  where answer = show(Response {version = "HTTP/1.1", statuscode = 404, headers=[]}) ++ "Page is not found"

respond :: Request -> Handle -> IO ()
respond request handle = do
  let redirect = lookup (path request) redirects
  if redirect /= Nothing
    then hPutStr handle $ show(Response {version = "HTTP/1.1", statuscode = 301, headers=["Location: " ++ fromJust redirect]})
    else do
      let filepath = (tail (path request))
      exists <- doesFileExist filepath
      if exists
        then do
          bfile <- B.readFile filepath
          let answer = B.append (C.pack (show(Response {version = "HTTP/1.1", statuscode = 200, headers=[]}))) bfile
          B.hPutStr handle $ answer
        else do
          answer <- case (path request) of
                  "/main" -> mainPage request
                  "/register" -> registerPage request
                  "/login" -> loginPage request
                  "/ajax" -> processAjax request
                  otherwise -> errorPage request
          hSetEncoding handle localeEncoding
          if (path request) /= "/404" 
            then hPutStr handle $ answer
            else print "error during request"
 

parseRequestHelper :: ([String], [(String,String)]) -> [(String,String)]
parseRequestHelper ([], accum) = accum
parseRequestHelper ((l:rest), accum) 
  | (length (words l)) < 2 = accum
  | otherwise = parseRequestHelper(rest, accum ++ [(reverse . tail . reverse . head . words $ l, unwords . tail . words $ l)] )

parseRequest :: [String] -> Request
parseRequest [] = Request {rtype = GET,
                          path = "/404",
                          options = [],
                          args = []}    --invalid request
parseRequest lns = case (words (head lns)) of
  [t,p,_] -> Request {rtype=(fromString t), 
                      path=(getDomain p), 
                      options=parseRequestHelper((tail lns),[]),
                      args= if t == "GET" 
                              then parsePath p
                              else do
                                let len = lookup "Content-Length" (parseRequestHelper((tail lns),[]))
                                tail2arglist (read (fromJust len)) (cutArgs lns)}

cutArgs:: [String] -> [String]
cutArgs [] = []
cutArgs (a:b) = case a of
  "\r" -> b
  otherwise -> cutArgs b

splitBy del str = helper del str []   
    where 
        helper _ [] acc = let acc0 = reverse acc in [acc0] 
        helper del (x:xs) acc   
            | x==del    = let acc0 = reverse acc in acc0 : helper del xs []  
            | otherwise = let acc0 = x : acc     in helper del xs acc0 

parseArgs n args = case args of 
  [] -> []
  _ -> splitBy '&' (take n (head $ args))

trim = f . f
   where f = reverse . dropWhile isSpace

list2Pair [f,s] = (trim f, s)
list2Pair [f] = (f, "")

tail2arglist n str = do
  let args = parseArgs n str
  let arglist = map (splitBy '=') args
  map list2Pair arglist 

parsePath str = do
  let baseDel = splitBy '?' str
  let base = head baseDel
  let tl = tail $ baseDel
  let args = parseArgs 4000 tl
  let arglist = map (splitBy '=') args
  map list2Pair arglist

getDomain str = do
  let baseDel = splitBy '?' str
  case baseDel of
    [] -> []
    otherwise -> head baseDel

parseCookie str = do
  let list = splitBy ';' str
  let modlist = map (splitBy '=') list
  map list2Pair modlist

handleAccept :: Handle -> String -> IO ()
handleAccept handle hostname = do 
  putStrLn $ "Handling request from " ++ hostname
  request <- fmap (parseRequest . lines) (hGetContents handle) 
  putStrLn (show request)
  respond request handle
  return ()
  
main = withSocketsDo $ do
  sock <- listenOn (PortNumber 5002)
  putStrLn "Listening on port 5002"
  forever $ do
    (handle, hostname, port) <- accept sock
    handleAccept handle hostname
    hClose handle

