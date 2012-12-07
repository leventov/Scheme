import Racket.Interpreter
import Racket.Parser (parseExprs)
import Racket.Core (newEnv)
import Network.WebSockets as WS
import Data.Text (Text, pack, unpack)
import Control.Monad (forever)
import Control.Concurrent.MVar
import Control.Applicative ((<$>))

import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import System.Environment (getArgs)

import Text.Printf (printf)

app :: MVar Int -> WS.Request -> WS.WebSockets WS.Hybi00 ()
app state rq = do
    clientNo <- liftIO $ takeMVar state
    liftIO $ putMVar state (clientNo + 1)
    WS.acceptRequest rq
    clientVersion <- WS.getVersion
    liftIO $ putStrLn $ printf "client %d: version: %s" clientNo clientVersion
    WS.sendTextData $ pack greeting
    let handleError e = liftIO $ putStrLn $
            printf "error while serving client %d: %s" clientNo (show e)
        talk env = flip WS.catchWsError handleError $ do
            exprs <- unpack <$> WS.receiveData 
            liftIO $ putStrLn $
                printf "client %d requested: %s" clientNo exprs
            let pr = parseExprs exprs
            liftIO $ putStrLn $ "parsed: " ++ show pr
            let (result, resultEnv) =
                    either (\e -> ("Parse error: " ++ show e, env))
                        (runExprs env) pr
                        
            liftIO $ putStrLn $
                printf "answer for client %d: %s" clientNo result
            WS.sendTextData $ pack result
            talk resultEnv
    talk $ newEnv builtins 50

main :: IO ()
main = do
    args <- getArgs
    let port = if null args then 9160 else read (head args) :: Int
    putStrLn ("server started at port " ++ show port)
    state <- newMVar 1
    WS.runServer "0.0.0.0" port $ app state