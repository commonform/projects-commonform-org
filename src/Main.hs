module Main (main) where

import Control.Exception (catch)
import Data.Maybe (fromMaybe)
import Data.Char (toLower)
import Network.Wai
import Network.Wai.Handler.Warp (run, Port)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import ExampleWAIServer (handler)
import System.Environment (getEnv)
import Text.Read (readMaybe)

-- Port

defaultPort :: Port
defaultPort = 8080

getPort :: IO Port
getPort = readPort `catch` useDefault where
    readPort = do
        port <- getEnv "PORT"
        return (fromMaybe defaultPort (readMaybe port))
    useDefault :: IOError -> IO Port
    useDefault = const (return defaultPort)

-- Environment

data Environment = Development | Production | Test
defaultEnvironment = Development

getEnvironment :: IO Environment
getEnvironment = readEnvironment `catch` useDefault where
    readEnvironment = do
        environment <- getEnv "ENV"
        let parsed = case map toLower environment of
                         "production" -> Production
                         "test" -> Test
                         _ -> defaultEnvironment
        return parsed
    useDefault :: IOError -> IO Environment
    useDefault = const (return defaultEnvironment)

loggingMiddleware :: Environment -> Middleware
loggingMiddleware Development = logStdoutDev
loggingMiddleware Production = logStdout
loggingMiddleware Test = id

-- Server

main :: IO ()
main = do
    port <- getPort
    environment <- getEnvironment
    putStrLn $ "Listening on port " ++ show port ++ "."
    run port $ loggingMiddleware environment handler
