module Main (main) where

-- Imports used to read from environment variables.
import Control.Exception (catch)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import System.Environment (getEnv)
import Text.Read (readMaybe)

-- Imports used to start a Warp HTTP server.
import Network.Wai
import Network.Wai.Handler.Warp (run, Port)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)

-- The request handler for the Warp server.
import ExampleWAIServer (handler)

-- Port

defaultPort :: Port
defaultPort = 8080

-- Read port number from the environment variable "PORT".

getPort :: IO Port
getPort =
    -- `getEnv` will throw an exception if "PORT" isn't set in the
    -- environment, so we need to catch that exception and provide a
    -- default port.
    readPort `catch` useDefault where
    readPort = do
        port <- getEnv "PORT"
        -- If the value of "PORT" can't be parsed to a string, use the
        -- default port.
        return (fromMaybe defaultPort (readMaybe port))
    -- Catch the exception when there is no "PORT" environment variable.
    useDefault :: IOError -> IO Port
    useDefault = const (return defaultPort)

-- Environment

-- There are three possible runtime environments.
data Environment = Development | Production | Test
defaultEnvironment = Development

-- Read runtime environment from the environment variable "ENV".

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
-- Middleware are functions that take a WAI handler argument and return
-- another WAI handler function. The `id` function just returns its
-- argument, so it works as middleware that doesn't change the handler.
loggingMiddleware Test = id

-- Server

main :: IO ()
main = do
    port <- getPort
    environment <- getEnvironment
    putStrLn $ "Listening on port " ++ show port ++ "."
    -- Run a Warp server using the request handler.
    run port $ loggingMiddleware environment handler
