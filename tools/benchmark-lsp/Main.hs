-- Copyright (C) 2020  Matthew Glazar
-- See end of file for extended copyright information.

-- @@@ run hindent

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Function (fix)
import qualified Data.Type.Equality as Equality
import qualified System.Console.ANSI as ANSI
import Control.Monad.Trans.State.Strict
import qualified LSPDecode as LSP
import Control.Applicative
import Control.Concurrent (ThreadId, forkIO, killThread)
import qualified LSPClient
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar, tryPutMVar)
import Control.DeepSeq (NFData(rnf))
import Control.Lens ((^.))
import Control.Monad (forever, forM, forM_, mapM_, void)
import Control.Monad.IO.Class
import Data.Dynamic (Dynamic, Typeable, fromDyn, toDyn)
import Data.Int (Int64)
import Data.Maybe (fromJust, fromMaybe)
import GHC.Generics (Generic)
import System.Directory (withCurrentDirectory)
import System.Exit (exitFailure)
import System.IO (stderr, hPutStrLn)
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import qualified Criterion.Main as Criterion
import qualified Criterion.Measurement as Criterion
import qualified Criterion.Measurement.Types as Criterion
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8    as BS
import qualified System.IO as IO
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Language.LSP.Test as LSP
import qualified Language.LSP.Types as LSP
import qualified Language.LSP.Types.Capabilities as LSP
import qualified Language.LSP.Types.Lens as LSP hiding (message)
import qualified System.Process as Process

main :: IO ()
main = do
  (lspClient, lspServerProcess) <- LSPClient.spawnLSPServer
    $ (Process.proc "/Users/strager/Projects/quick-lint-js/tools/benchmark-lsp/flow/./node_modules/.bin/flow" ["lsp"]) {Process.cwd = Just "flow/", Process.std_err = Process.Inherit}
    -- $ (Process.proc "node" ["./node_modules/eslint-server/lib/index.js", "--stdio"]) {Process.cwd = Just "eslint/", Process.std_err = Process.Inherit}
    -- $ (Process.proc "/Users/strager/Projects/quick-lint-js/build/quick-lint-js" ["--lsp-server"]) {Process.std_err = Process.Inherit}

  -- @@@ use cmd line arg for lspClientLogTraffic
  let lspClient' = lspClient { LSPClient.lspClientLogTraffic = Just stderr }

{-
  ((), lspClient'') <- flip runStateT lspClient' $ do
    initializeLSP

    LSPClient.sendNotification LSP.STextDocumentDidOpen $ LSP.DidOpenTextDocumentParams
      $ LSP.TextDocumentItem (LSP.filePathToUri "/Users/strager/Projects/quick-lint-js/tools/benchmark-lsp/eslint/hello.js") "javascript" 0 "let x, x;"

    waitForDiagnosticsOrTimeout (10*seconds) >>= \case
      Just diagnostics -> liftIO $ print diagnostics
      Nothing -> fail "Expected diagnostics but received none"

    shutDownLSP
-}

  ((), lspClient'') <- flip runStateT lspClient' $ do
    initializeLSP

    let version = 0
    let uri = LSP.filePathToUri "/Users/strager/Projects/quick-lint-js/tools/benchmark-lsp/flow/hello.js"
    LSPClient.sendNotification LSP.STextDocumentDidOpen $ LSP.DidOpenTextDocumentParams
      $ LSP.TextDocumentItem uri "javascript" version ""

    let seconds = 10^6

    waitForDiagnosticsOrTimeout (1*seconds) >>= \case
      Just diagnostics -> liftIO $ print diagnostics
      Nothing -> liftIO $ print "got no diags"

    let version' = version + 1
    LSPClient.sendNotification LSP.STextDocumentDidChange $ LSP.DidChangeTextDocumentParams
      (LSP.VersionedTextDocumentIdentifier uri (Just version))
      (LSP.List [LSP.TextDocumentContentChangeEvent Nothing Nothing "let x, x;"])

    waitForDiagnosticsOrTimeout (10*seconds) >>= \case
      Just diagnostics -> liftIO $ print diagnostics
      Nothing -> fail "Expected diagnostics but received none"

    shutDownLSP


  Process.cleanupProcess lspServerProcess
  return ()

waitForDiagnosticsOrTimeout :: Int -> StateT LSPClient.LSPClient IO (Maybe LSP.PublishDiagnosticsParams)
waitForDiagnosticsOrTimeout timeoutMicroseconds
  = fix $ \loop -> LSPClient.receiveMessageWithTimeout timeoutMicroseconds >>= \case
        Just (LSPClient.matchNotification LSP.STextDocumentPublishDiagnostics -> Just parameters) -> return $ Just parameters
        Just (matchMiscMessage -> Just handle) -> handle >> loop
        Nothing -> return Nothing

initializeLSP :: StateT LSPClient.LSPClient IO ()
initializeLSP = do
    let clientCapabilities = LSP.ClientCapabilities Nothing Nothing Nothing Nothing
    -- Flow's LSP server requires rootPath.
    -- @@@ don't hard-code
    let rootPath = "/Users/strager/Projects/quick-lint-js/tools/benchmark-lsp/flow"
    initializeID <- LSPClient.sendRequest LSP.SInitialize $ LSP.InitializeParams Nothing Nothing Nothing (Just rootPath) Nothing Nothing clientCapabilities Nothing Nothing
    fix $ \loop -> LSPClient.receiveMessage >>= \case
      (LSPClient.matchResponse LSP.SInitialize initializeID -> Just (Right _)) -> return ()
      (LSPClient.matchAnyNotification -> True) -> loop
    LSPClient.sendNotification LSP.SInitialized $ Just LSP.InitializedParams

shutDownLSP :: StateT LSPClient.LSPClient IO () 
shutDownLSP = do
    shutdownID <- LSPClient.sendRequest LSP.SShutdown LSP.Empty
    fix $ \loop -> LSPClient.receiveMessage >>= \case
      (LSPClient.matchResponse LSP.SShutdown shutdownID -> Just (Right _)) -> return ()
      (matchMiscMessage -> Just handle) -> handle >> loop
    LSPClient.sendNotification LSP.SExit LSP.Empty

matchMiscMessage
  :: LSP.FromServerMessage
  -> Maybe (StateT LSPClient.LSPClient IO ())
matchMiscMessage = \case
  (LSPClient.matchRequest LSP.SClientRegisterCapability -> Just (requestID, _request)) -> Just $ do
    LSPClient.sendResponse requestID LSP.Empty
  (LSPClient.matchRequest LSP.SWorkspaceConfiguration -> Just (requestID, request)) -> Just $ do
          -- HACK(strager): eslint-server breaks if we don't give all of these options.
          let (LSP.List requestedItems) = request ^. LSP.items
          let configurations = map (\_item -> Aeson.Object $ HashMap.fromList
                  -- For eslint-server:
                  [ ("run", Aeson.String "onType")
                  , ("validate", Aeson.String "probe")
                  ]) requestedItems
          LSPClient.sendResponse requestID (LSP.List configurations)
  (LSPClient.matchAnyNotification -> True) -> Just $ return ()
  _ -> Nothing

{-
main :: IO ()
main = do
  benchmarkConfigOrError <- Aeson.eitherDecodeFileStrict "benchmark-config.json" :: IO (Either String BenchmarkConfig)
  benchmarkConfig <- case benchmarkConfigOrError of
    Left error -> do
      hPutStrLn stderr error
      exitFailure
    Right benchmarkConfig -> return benchmarkConfig
  Criterion.defaultMain $ map benchmarkLSPServer (benchmarkConfigServers benchmarkConfig)

benchmarkLSPServer :: BenchmarkConfigServer -> Criterion.Benchmark
benchmarkLSPServer serverConfig@BenchmarkConfigServer{..} = Criterion.bgroup benchmarkConfigServerName
  [ benchOpenWaitClose
  , benchChangeWait
  ]
  where
    -- TODO(strager): Add a similar benchmark with a warmup phase.
    benchOpenWaitClose :: Criterion.Benchmark
    benchOpenWaitClose = benchmarkWithLSPServer "open-wait-close" serverConfig setUp run
        where
          setUp batchSize = return batchSize

          run batchSize = forM_ [1..batchSize] $ \_ -> do
              document <- LSP.createDoc "hello.js" "javascript" source
              let go
                    = lspGetDiagnostics
                    <|> (lspRespondToRegisterCapabilityRequest *> go)
                    <|> (lspRespondToWorkspaceConfigurationRequest *> go)
                    <|> (void LSP.anyNotification *> go)
              go
              -- @@@ assert that we get at least one diag. TypeScript fails.
              LSP.closeDoc document

          source :: Text.Text
          source = "let x, x;"

    benchChangeWait :: Criterion.Benchmark
    benchChangeWait = benchmarkWithLSPServer "change-wait" serverConfig setUp run
      where
        setUp batchSize = do
          documents <- forM [1..batchSize] $ \i -> do
            document <- LSP.createDoc ("hello" <> show i <> ".js") "javascript" initialSource
            liftIO $ putStrLn "yo"
            deadline <- lspBeginTimeout 0.5
            liftIO $ putStrLn "yo2"
            let go
                  = void lspGetDiagnostics
                  <|> (lspRespondToRegisterCapabilityRequest *> go)
                  <|> (lspRespondToWorkspaceConfigurationRequest *> go)
                  <|> (void LSP.anyNotification *> go)
                  <|> (lspPollTimeout deadline *> go)
            go
            return document
          return documents

        run documents = forM_ documents $ \document -> do
            LSP.changeDoc document [LSP.TextDocumentContentChangeEvent Nothing Nothing source]
            let go
                  = lspGetDiagnostics
                  <|> (void LSP.anyNotification *> go)
            go
            -- @@@ assert that we get at least one diag. TypeScript fails.

        initialSource :: Text.Text
        initialSource = ""

        source :: Text.Text
        source = "let x, x;"


lspGetDiagnostics :: LSP.Session [LSP.Diagnostic]
lspGetDiagnostics = do
  liftIO $ putStrLn "gonna lspGetDiagnostics"
  notification <- LSP.message LSP.STextDocumentPublishDiagnostics
  liftIO $ putStrLn "got lspGetDiagnostics"
  let (LSP.List diagnostics) = notification ^. LSP.params . LSP.diagnostics
  return diagnostics

lspRespondToRegisterCapabilityRequest :: LSP.Session ()
lspRespondToRegisterCapabilityRequest = do
  liftIO $ putStrLn "gonna lspRespondToRegisterCapabilityRequest"
  request <- LSP.message LSP.SClientRegisterCapability
  -- HACK(strager): We should respond with MethodNotFound, but eslint-server
  -- breaks if we don't give a successful response.
  lspSendResponse request LSP.Empty

lspRespondToWorkspaceConfigurationRequest :: LSP.Session ()
lspRespondToWorkspaceConfigurationRequest = do
  request <- LSP.message LSP.SWorkspaceConfiguration
  -- HACK(strager): eslint-server breaks if we don't give all of these options.
  let (LSP.List requestedItems) = request ^. LSP.params ^. LSP.items
  let configurations = map (\_item -> Aeson.Object $ HashMap.fromList
        -- For eslint-server:
        [ ("run", Aeson.String "onType")
        , ("validate", Aeson.String "probe")
        ]) requestedItems
  lspSendResponse request (LSP.List configurations)

lspSendResponse request response = LSP.sendResponse $ LSP.ResponseMessage
  (request ^. LSP.jsonrpc)
  (Just (request ^. LSP.id))
  (Right response)

benchmarkWithLSPServer
  :: (NFData a, NFData env, Typeable a, Typeable env)
  => String
  -- ^ Benchmark name.
  -> BenchmarkConfigServer
  -- ^ LSP server configuration.
  -> (Int64 -> LSP.Session env)
  -- ^ Setup. Given batch size.
  -> (env -> LSP.Session a)
  -- ^ Work. Given env returned by setup.
  -> Criterion.Benchmark
benchmarkWithLSPServer name BenchmarkConfigServer{..} setUp work =
  Criterion.bench name $ Criterion.Benchmarkable alloc clean work' cleanUpEachRun
  where
    alloc batchSize = do
      server <- startLSPServer benchmarkConfigServerCommand (fromMaybe "." benchmarkConfigServerCWD)
      env <- withLSP server (setUp batchSize)
      return (server, env)

    clean batchSize (server, _env) = stopLSPServer server

    work' ~(server, env) _batchSize = do
      result <- withLSP server (work env)
      rnf result `seq` return ()

    cleanUpEachRun = False

data BenchmarkConfig = BenchmarkConfig
  { benchmarkConfigServers :: [BenchmarkConfigServer]
  }

data BenchmarkConfigServer = BenchmarkConfigServer
  { benchmarkConfigServerName :: String
  , benchmarkConfigServerCommand :: String
  , benchmarkConfigServerCWD :: Maybe FilePath
  }

instance Aeson.FromJSON BenchmarkConfig where
    parseJSON = Aeson.withObject "BenchmarkConfig" $ \v -> BenchmarkConfig
        <$> v Aeson..: "servers"

instance Aeson.FromJSON BenchmarkConfigServer where
    parseJSON = Aeson.withObject "BenchmarkConfigServer" $ \v -> BenchmarkConfigServer
        <$> v Aeson..: "name"
        <*> v Aeson..: "command"
        <*> v Aeson..:? "cwd"

lspBeginTimeout :: Double -> LSP.Session Double
lspBeginTimeout duration = do
  now <- liftIO Criterion.getTime
  return $ now + duration

lspPollTimeout :: Double -> LSP.Session ()
lspPollTimeout deadline = do
  -- @@@ this timeout stuff ain't working. prints aren't happening. =\
  liftIO $ putStrLn "uh"
  now <- liftIO Criterion.getTime
  if now >= deadline
  --then return ()
  then liftIO $ putStrLn "done"
  else do
    liftIO $ putStrLn "poll"
    liftIO $ Concurrent.threadDelay 10
    liftIO $ putStrLn "polldone"
    empty
    liftIO $ putStrLn "!!! after empty"

-- | Start an LSP server in another thread.
startLSPServer :: String -> FilePath -> IO LSPServer
startLSPServer command serverCWD = do
  serverReady <- newEmptyMVar :: IO (MVar (Maybe Exception.SomeException))
  workMVar <- newEmptyMVar :: IO (MVar (LSP.Session Dynamic))
  workDoneMVar <- newEmptyMVar :: IO (MVar (Either Exception.SomeException Dynamic))

  withCurrentDirectory serverCWD $ do
    let clientCapabilities = LSP.ClientCapabilities Nothing Nothing Nothing Nothing
    let sessionConfig = LSP.defaultConfig { LSP.messageTimeout = 5 } -- @@@ 5
    let threadRoutine = LSP.runSessionWithConfig sessionConfig command clientCapabilities "." $ do
          liftIO $ putMVar serverReady Nothing
          forever $ do
            work <- liftIO $ takeMVar workMVar
            workResult <- work
            liftIO $ putMVar workDoneMVar (Right workResult)
    let propagateException e = do
          _ <- tryPutMVar serverReady (Just e)
          _ <- tryPutMVar workDoneMVar (Left e)
          return ()
    threadID <- forkIO $ Exception.catch threadRoutine propagateException

    maybeError <- takeMVar serverReady
    case maybeError of
      Nothing -> return LSPServer { lspServerThreadID = threadID, lspServerWorkMVar = workMVar, lspServerWorkDoneMVar = workDoneMVar }
      Just error -> Exception.throw error

stopLSPServer :: LSPServer -> IO ()
stopLSPServer server = killThread (lspServerThreadID server)

withLSP :: (Typeable a) => LSPServer -> LSP.Session a -> IO a
withLSP server work = do
  let work' = (toDyn <$> work) :: LSP.Session Dynamic
  putMVar (lspServerWorkMVar server) work'
  workResult <- takeMVar (lspServerWorkDoneMVar server) :: IO (Either Exception.SomeException Dynamic)
  case workResult of
    Left error -> Exception.throw error
    Right result -> return $ fromDyn result (error "withLSP: type mismatch")

data LSPServer = LSPServer
  { lspServerThreadID :: ThreadId
  , lspServerWorkMVar :: MVar (LSP.Session Dynamic)
  , lspServerWorkDoneMVar :: MVar (Either Exception.SomeException Dynamic)
  }
  deriving (Generic, NFData)

instance NFData LSP.TextDocumentIdentifier where
  rnf x = rnf (x ^. LSP.uri)
-}

-- quick-lint-js finds bugs in JavaScript programs.
-- Copyright (C) 2020  Matthew Glazar
--
-- This file is part of quick-lint-js.
--
-- quick-lint-js is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- quick-lint-js is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with quick-lint-js.  If not, see <https://www.gnu.org/licenses/>.
