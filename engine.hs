import Network
import Control.Concurrent
import System.IO
import System.Directory 

main = withSocketsDo $ do
    sock <- listenOn $ PortNumber 5002
    loop sock
 
loop sock = do
   (h,_,_) <- accept sock
   forkIO $ body h
   loop sock
  where
   body h = do
       d <- getCurrentDirectory
       hPutStr h ("HTTP/1.0 200 OK\r\nContent-Length: " ++ show(length(show(d))) ++ "\r\n\r\n" ++ show (d) ++ "\r\n")
       hFlush h
       hClose h
   
