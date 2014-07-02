{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Import
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Network.Wai.Middleware.RequestLogger
    ( mkRequestLogger, outputFormat, OutputFormat (..), IPAddrSource (..), destination
    )
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import Network.HTTP.Conduit (newManager, conduitManagerSettings)
import Control.Concurrent (forkIO, threadDelay)
import System.Log.FastLogger (newStdoutLoggerSet, defaultBufSize)
import Network.Wai.Logger (clockDateCacher)
import Data.Default (def)
import Yesod.Core.Types (loggerSet, Logger (Logger))
import Network.Wai.Middleware.MethodOverride (methodOverride)

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Home
import Handler.User
import Handler.Doc
import Handler.Wallet
import Handler.Card
import Handler.Account
import Handler.Transaction

import Data.IORef (newIORef)

import Yesod.MangoPay

import qualified Data.Map as M
import System.IO (stdout, hFlush)

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO Application
makeApplication conf = do
    foundation <- makeFoundation conf

    -- Initialize the logging middleware
    logWare <- mkRequestLogger def
        { outputFormat =
            if development
                then Detailed True
                else Apache FromSocket
        , destination = RequestLogger.Logger $ loggerSet $ appLogger foundation
        }

    -- register all notification callbacks
    -- we need a handler to resolve the Route URL
    -- so we use runFakeHandler, because all the caveats the doc outlines don't apply to us
    -- we don't need anything from the request, nor do we change anything in our state
    err<-runFakeHandler M.empty appLogger foundation (registerAllMPCallbacks MPHookR)
    print err
    hFlush stdout

    -- Create the WAI application and apply middlewares
    -- Using MethodOverride middleware to allow PUT forms see:
    -- http://stackoverflow.com/questions/22902419
    app <- toWaiAppPlain foundation
    return $ logWare $ methodOverride app

-- | Loads up any necessary settings, creates your foundation datatype, and
-- performs some initialization.
makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- newManager conduitManagerSettings
    s <- staticSite
    loggerSet' <- newStdoutLoggerSet defaultBufSize
    (getter, updater) <- clockDateCacher
    iorToken<-newIORef Nothing
    iorEvents<-newIORef []
    -- If the Yesod logger (as opposed to the request logger middleware) is
    -- used less than once a second on average, you may prefer to omit this
    -- thread and use "(updater >> getter)" in place of "getter" below.  That
    -- would update the cache every time it is used, instead of every second.
    let updateLoop = do
            threadDelay 1000000
            updater
            updateLoop
    _ <- forkIO updateLoop

    let logger = Yesod.Core.Types.Logger loggerSet' getter
        foundation = App conf s manager logger iorToken iorEvents

    return foundation

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
