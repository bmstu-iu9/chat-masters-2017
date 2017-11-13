import Control.Monad
import Data.Char
import System.IO
import Network
import Data.Time.LocalTime
import Data.Maybe
import Data.Char
import Debug.Trace

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

mainPage request = answer where
  cookies = lookup "Cookie" (options(request))
  cookiesList = if cookies /= Nothing
    then parseCookie (fromJust cookies)
    else []
  user_id = lookup "user_id" cookiesList
  answer = if user_id /= Nothing
   then show(Response {version = "HTTP/1.1", statuscode = 200, headers=[]}) ++ "Main page is under developing"
   else show(Response {version = "HTTP/1.1", statuscode = 303, headers=["Location: /login"]})

registerPage request = answer
  where answer = show(Response {version = "HTTP/1.1", statuscode = 200, headers=[]}) ++ "Register page is under developing"

loginPage request = answer where
  login = lookup "login" (args(request))
  answer = if login /= Nothing
    then show(Response {version = "HTTP/1.1", statuscode = 200, headers=["Set-Cookie: user_id=" ++ fromJust login]}) ++ "Autorized successfully"
    else show(Response {version = "HTTP/1.1", statuscode = 200, headers=[]}) ++ "Login page is under developing"

errorPage request = answer
  where answer = show(Response {version = "HTTP/1.1", statuscode = 404, headers=[]}) ++ "Page is not found"

respond :: Request -> Handle -> IO ()
respond request handle = hPutStr handle $ answer where
  redirect = lookup (path request) redirects  
  answer = if redirect /= Nothing
     then show(Response {version = "HTTP/1.1", statuscode = 301, headers=["Location: " ++ fromJust redirect]})
     else case (path request) of
      "/main" -> mainPage request
      "/register" -> registerPage request
      "/login" -> loginPage request
      otherwise -> errorPage request

parseRequestHelper :: ([String], [(String,String)]) -> [(String,String)]
parseRequestHelper ([], accum) = accum
parseRequestHelper ((l:rest), accum) 
  | (length (words l)) < 2 = accum
  | otherwise = parseRequestHelper(rest, accum ++ [(reverse . tail . reverse . head . words $ l, unwords . tail . words $ l)] )

parseRequest :: [String] -> Request
parseRequest lns = case (words (head lns)) of
  [t,p,_] -> Request {rtype=(fromString t), path=(getDomain p), args=(parsePath p), options=parseRequestHelper((tail lns),[])}

splitBy del str = helper del str []   
    where 
        helper _ [] acc = let acc0 = reverse acc in [acc0] 
        helper del (x:xs) acc   
            | x==del    = let acc0 = reverse acc in acc0 : helper del xs []  
            | otherwise = let acc0 = x : acc     in helper del xs acc0 

parseArgs args = case args of 
  [] -> []
  _ -> splitBy '&' (head $ args)

trim = f . f
   where f = reverse . dropWhile isSpace

list2Pair [f,s] = (trim f, s)
list2Pair [f] = (f, "")

parsePath str = do
  let baseDel = splitBy '?' str
  let base = head baseDel
  let tl = tail $ baseDel
  let args = parseArgs tl
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