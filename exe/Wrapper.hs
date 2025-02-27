{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExplicitNamespaces         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
-- | This module is based on the hie-wrapper.sh script in
-- https://github.com/alanz/vscode-hie-server
module Main where

import           Control.Monad.Extra
import           Data.Char                          (isSpace)
import           Data.Default
import           Data.Either.Extra                  (eitherToMaybe)
import           Data.Foldable
import           Data.List
import           Data.Void
import qualified Development.IDE.Session            as Session
import qualified HIE.Bios.Environment               as HieBios
import           HIE.Bios.Types
import           Ide.Arguments
import           Ide.Version
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.Info
import           System.IO
#ifndef mingw32_HOST_OS
import qualified Data.Map.Strict                    as Map
import           System.Posix.Process               (executeFile)
#else
import           System.Process
#endif
import           Control.Concurrent                 (tryPutMVar)
import           Control.Monad.IO.Class             (MonadIO (liftIO))
import           Control.Monad.IO.Unlift            (MonadUnliftIO)
import           Control.Monad.Trans.Except         (ExceptT, runExceptT,
                                                     throwE)
import           Data.Maybe
import qualified Data.Text                          as T
import qualified Data.Text.IO                       as T
import           Development.IDE.LSP.LanguageServer (runLanguageServer)
import qualified Development.IDE.Main               as Main
import           Development.IDE.Types.Logger       (Logger (Logger),
                                                     Pretty (pretty),
                                                     Priority (Debug, Error, Info, Warning),
                                                     Recorder (logger_),
                                                     WithPriority (WithPriority),
                                                     cmapWithPrio,
                                                     makeDefaultStderrRecorder)
import           GHC.Stack.Types                    (emptyCallStack)
import           HIE.Bios.Internal.Log
import           Ide.Plugin.Config                  (Config)
import           Language.LSP.Server                (LspM)
import qualified Language.LSP.Server                as LSP
import           Language.LSP.Types                 (MessageActionItem (MessageActionItem),
                                                     MessageType (MtError),
                                                     Method (Initialize),
                                                     RequestMessage,
                                                     ResponseError,
                                                     SMethod (SExit, SWindowShowMessageRequest),
                                                     ShowMessageRequestParams (ShowMessageRequestParams))

-- ---------------------------------------------------------------------

main :: IO ()
main = do
  -- WARNING: If you write to stdout before runLanguageServer
  --          then the language server will not work
  args <- getArguments "haskell-language-server-wrapper" mempty

  hlsVer <- haskellLanguageServerVersion
  case args of
      ProbeToolsMode -> do
          programsOfInterest <- findProgramVersions
          putStrLn hlsVer
          putStrLn "Tool versions found on the $PATH"
          putStrLn $ showProgramVersionOfInterest programsOfInterest
          putStrLn "Tool versions in your project"
          cradle <- findProjectCradle' False
          ghcVersion <- runExceptT $ getRuntimeGhcVersion' cradle
          putStrLn $ showProgramVersion "ghc" $ mkVersion =<< eitherToMaybe ghcVersion

      VersionMode PrintVersion ->
          putStrLn hlsVer

      VersionMode PrintNumericVersion ->
          putStrLn haskellLanguageServerNumericVersion

      BiosMode PrintCradleType ->
          print =<< findProjectCradle
      PrintLibDir -> do
          cradle <- findProjectCradle' False
          (CradleSuccess libdir) <- HieBios.getRuntimeGhcLibDir cradle
          putStr libdir
      _ -> launchHaskellLanguageServer args >>= \case
            Right () -> pure ()
            Left err -> do
              T.hPutStrLn stderr (prettyError err NoShorten)
              case args of
                Ghcide _ -> launchErrorLSP (prettyError err Shorten)
                _        -> pure ()

launchHaskellLanguageServer :: Arguments -> IO (Either WrapperSetupError ())
launchHaskellLanguageServer parsedArgs = do
  case parsedArgs of
    Ghcide GhcideArguments{..} -> whenJust argsCwd setCurrentDirectory
    _                          -> pure ()

  d <- getCurrentDirectory

  -- search for the project cradle type
  cradle <- findProjectCradle

  -- Get the root directory from the cradle
  setCurrentDirectory $ cradleRootDir cradle

  case parsedArgs of
    Ghcide GhcideArguments{..} ->
      when argsProjectGhcVersion $ do
        runExceptT (getRuntimeGhcVersion' cradle) >>= \case
          Right ghcVersion -> putStrLn ghcVersion >> exitSuccess
          Left err -> T.putStrLn (prettyError err NoShorten) >> exitFailure
    _ -> pure ()

  progName <- getProgName
  hPutStrLn stderr $ "Run entered for haskell-language-server-wrapper(" ++ progName ++ ") "
                     ++ hlsVersion
  hPutStrLn stderr $ "Current directory: " ++ d
  hPutStrLn stderr $ "Operating system: " ++ os
  args <- getArgs
  hPutStrLn stderr $ "Arguments: " ++ show args
  hPutStrLn stderr $ "Cradle directory: " ++ cradleRootDir cradle
  hPutStrLn stderr $ "Cradle type: " ++ show (actionName (cradleOptsProg cradle))
  programsOfInterest <- findProgramVersions
  hPutStrLn stderr ""
  hPutStrLn stderr "Tool versions found on the $PATH"
  hPutStrLn stderr $ showProgramVersionOfInterest programsOfInterest
  hPutStrLn stderr ""
  -- Get the ghc version -- this might fail!
  hPutStrLn stderr "Consulting the cradle to get project GHC version..."

  runExceptT $ do
      ghcVersion <- getRuntimeGhcVersion' cradle
      liftIO $ hPutStrLn stderr $ "Project GHC version: " ++ ghcVersion

      let
        hlsBin = "haskell-language-server-" ++ ghcVersion
        candidates' = [hlsBin, "haskell-language-server"]
        candidates = map (++ exeExtension) candidates'

      liftIO $ hPutStrLn stderr $ "haskell-language-server exe candidates: " ++ show candidates

      mexes <- liftIO $ traverse findExecutable candidates

      case asum mexes of
        Nothing -> throwE (NoLanguageServer ghcVersion candidates)
        Just e -> do
          liftIO $ hPutStrLn stderr $ "Launching haskell-language-server exe at:" ++ e

#ifdef mingw32_HOST_OS
          liftIO $ callProcess e args
#else

          let Cradle { cradleOptsProg = CradleAction { runGhcCmd } } = cradle

          let cradleName = actionName (cradleOptsProg cradle)
          -- we need to be compatible with NoImplicitPrelude
          ghcBinary <- liftIO (fmap trim <$> runGhcCmd ["-v0", "-package-env=-", "-ignore-dot-ghci", "-e", "Control.Monad.join (Control.Monad.fmap System.IO.putStr System.Environment.getExecutablePath)"])
                         >>= cradleResult cradleName

          libdir <- liftIO (HieBios.getRuntimeGhcLibDir cradle)
                      >>= cradleResult cradleName

          env <- Map.fromList <$> liftIO getEnvironment
          let newEnv = Map.insert "GHC_BIN" ghcBinary $ Map.insert "GHC_LIBDIR" libdir env
          liftIO $ executeFile e True args (Just (Map.toList newEnv))
#endif



cradleResult :: ActionName Void -> CradleLoadResult a -> ExceptT WrapperSetupError IO a
cradleResult _ (CradleSuccess ver) = pure ver
cradleResult cradleName (CradleFail error) = throwE $ FailedToObtainGhcVersion cradleName error
cradleResult cradleName CradleNone = throwE $ NoneCradleGhcVersion cradleName

-- | Version of 'getRuntimeGhcVersion' that dies if we can't get it, and also
-- checks to see if the tool is missing if it is one of
getRuntimeGhcVersion' :: Cradle Void -> ExceptT WrapperSetupError IO String
getRuntimeGhcVersion' cradle = do
  let cradleName = actionName (cradleOptsProg cradle)

  -- See if the tool is installed
  case cradleName of
    Stack   -> checkToolExists "stack"
    Cabal   -> checkToolExists "cabal"
    Default -> checkToolExists "ghc"
    Direct  -> checkToolExists "ghc"
    _       -> pure ()

  ghcVersionRes <- liftIO $ HieBios.getRuntimeGhcVersion cradle
  cradleResult cradleName ghcVersionRes

  where
    checkToolExists exe = do
      exists <- liftIO $ findExecutable exe
      case exists of
        Just _ -> pure ()
        Nothing -> throwE $ ToolRequirementMissing exe (actionName (cradleOptsProg cradle))

findProjectCradle :: IO (Cradle Void)
findProjectCradle = findProjectCradle' True

findProjectCradle' :: Bool -> IO (Cradle Void)
findProjectCradle' log = do
  d <- getCurrentDirectory

  let initialFp = d </> "a"
  hieYaml <- Session.findCradle def initialFp

  -- Some log messages
  when log $
      case hieYaml of
        Just yaml -> hPutStrLn stderr $ "Found \"" ++ yaml ++ "\" for \"" ++ initialFp ++ "\""
        Nothing -> hPutStrLn stderr "No 'hie.yaml' found. Try to discover the project type!"

  Session.loadCradle def hieYaml d

trim :: String -> String
trim s = case lines s of
  [] -> s
  ls -> dropWhileEnd isSpace $ last ls

data WrapperSetupError
    = FailedToObtainGhcVersion (ActionName Void) CradleError
    | NoneCradleGhcVersion (ActionName Void)
    | NoLanguageServer String [FilePath]
    | ToolRequirementMissing String (ActionName Void)
    deriving (Show)

data Shorten = Shorten | NoShorten

-- | Pretty error message displayable to the future.
-- Extra argument 'Shorten' can be used to shorten error message.
-- Reduces usefulness, but allows us to show the error message via LSP
-- as LSP doesn't allow any newlines and makes it really hard to read
-- the message otherwise.
prettyError :: WrapperSetupError -> Shorten ->  T.Text
prettyError (FailedToObtainGhcVersion name crdlError) shorten =
  "Failed to find the GHC version of this " <> T.pack (show name) <> " project." <>
  case shorten of
    Shorten ->
      "\n" <> T.pack (fromMaybe "" . listToMaybe $ cradleErrorStderr crdlError)
    NoShorten ->
      "\n" <> T.pack (intercalate "\n" (cradleErrorStderr crdlError))
prettyError (NoneCradleGhcVersion name) _ =
  "Failed to get the GHC version of this " <> T.pack (show name) <>
  " project because a none cradle is configured"
prettyError (NoLanguageServer ghcVersion candidates) _ =
  "Failed to find a HLS version for GHC " <> T.pack ghcVersion <>
  "\nExecutable names we failed to find: " <> T.pack (intercalate "," candidates)
prettyError (ToolRequirementMissing toolExe name) _ =
  "Failed to find executable \"" <> T.pack toolExe <> "\" in $PATH for this " <> T.pack (show name) <> " project."

newtype ErrorLSPM c a = ErrorLSPM { unErrorLSPM :: (LspM c) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, LSP.MonadLsp c)

-- | Launches a LSP that displays an error and presents the user with a request
-- to shut down the LSP.
launchErrorLSP :: T.Text -> IO ()
launchErrorLSP errorMsg = do
  recorder <- makeDefaultStderrRecorder Nothing Info

  let logger = Logger $ \p m -> logger_ recorder (WithPriority p emptyCallStack (pretty m))

  let defaultArguments = Main.defaultArguments (cmapWithPrio pretty recorder) logger

  inH <- Main.argsHandleIn defaultArguments

  outH <- Main.argsHandleOut defaultArguments

  let onConfigurationChange cfg _ = Right cfg

  let setup clientMsgVar = do
        -- Forcefully exit
        let exit = void $ tryPutMVar clientMsgVar ()

        let doInitialize :: LSP.LanguageContextEnv Config -> RequestMessage Initialize -> IO (Either ResponseError (LSP.LanguageContextEnv Config, ()))
            doInitialize env _ = do

              let restartTitle = "Try to restart"
              void $ LSP.runLspT env $ LSP.sendRequest SWindowShowMessageRequest (ShowMessageRequestParams MtError errorMsg (Just [MessageActionItem restartTitle])) $ \case
                    Right (Just (MessageActionItem title))
                       | title == restartTitle -> liftIO exit
                    _ -> pure ()

              pure (Right (env, ()))

        let asyncHandlers = mconcat
              [ exitHandler exit ]

        let interpretHandler (env,  _st) = LSP.Iso (LSP.runLspT env . unErrorLSPM) liftIO
        pure (doInitialize, asyncHandlers, interpretHandler)

  runLanguageServer (cmapWithPrio pretty recorder)
    (Main.argsLspOptions defaultArguments)
    inH
    outH
    (Main.argsDefaultHlsConfig defaultArguments)
    onConfigurationChange
    setup

exitHandler :: IO () -> LSP.Handlers (ErrorLSPM c)
exitHandler exit = LSP.notificationHandler SExit $ const $ liftIO exit

hlsWrapperLogger :: Logger
hlsWrapperLogger = Logger $ \pri txt ->
    case pri of
      Debug   -> debugm   (T.unpack txt)
      Info    -> logm     (T.unpack txt)
      Warning -> warningm (T.unpack txt)
      Error   -> errorm   (T.unpack txt)
