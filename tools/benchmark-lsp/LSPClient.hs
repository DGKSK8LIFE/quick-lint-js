-- Copyright (C) 2020  Matthew Glazar
-- See end of file for extended copyright information.

-- @@@ run hindent

-- @@@ trim
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module LSPClient where

import qualified Data.Type.Equality as Equality
import qualified System.Console.ANSI as ANSI
import Control.Monad.Trans.State.Strict
import qualified LSPDecode as LSP
import Control.Applicative
import Control.Concurrent (ThreadId, forkIO, killThread)
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

data LSPClient = LSPClient
  { lspClientServerStdin :: IO.Handle
  , lspClientServerStdout :: IO.Handle
  , lspClientRequestMap :: LSP.RequestMap
  , lspClientNextRequestID :: Int
  , lspClientLogTraffic :: Maybe IO.Handle
  }

makeLSPClient :: IO.Handle -> IO.Handle -> LSPClient
makeLSPClient serverStdin serverStdout = LSPClient
  { lspClientServerStdin = serverStdin
  , lspClientServerStdout = serverStdout
  , lspClientRequestMap = LSP.newRequestMap
  , lspClientNextRequestID = 0
  , lspClientLogTraffic = Nothing
  }

type SpawnedProcess = (Maybe IO.Handle, Maybe IO.Handle, Maybe IO.Handle, Process.ProcessHandle)

spawnLSPServer :: Process.CreateProcess -> IO (LSPClient, SpawnedProcess)
spawnLSPServer proc = do 
  lspServerProcess@(Just lspServerStdin, Just lspServerStdout, _lspServerStderr, _lspServerHandle) <- Process.createProcess
    proc
      { Process.std_in = Process.CreatePipe
      , Process.std_out = Process.CreatePipe
      , Process.std_err = Process.Inherit
      } 
  return (makeLSPClient lspServerStdin lspServerStdout, lspServerProcess)

receiveMessage :: StateT LSPClient IO LSP.FromServerMessage
receiveMessage = do
  client@LSPClient{..} <- get

  encodedMessage <- liftIO $ LSP.getNextMessage lspClientServerStdout
  case lspClientLogTraffic of
    Just handle -> liftIO $ do
      ANSI.hSetSGR handle [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Green]
      hPutStrLn handle $ "<~~ recv: " ++ BS.unpack encodedMessage
      ANSI.hSetSGR handle [ANSI.Reset]
    Nothing -> return ()

  let (requestMap', message) = LSP.decodeFromServerMsg lspClientRequestMap encodedMessage
  put client { lspClientRequestMap = requestMap' }
  return message

matchResponse
  :: forall (m :: LSP.Method LSP.FromClient LSP.Request).
     LSP.SMethod m
  -> LSP.LspId m
  -> LSP.FromServerMessage 
  -> Maybe (Either LSP.ResponseError (LSP.ResponseResult m))
matchResponse method id message = case message of
          LSP.FromServerRsp responseMethod response@(LSP.ResponseMessage _ responseID responseResult) ->
            case method `LSP.mEqClient` responseMethod of
              Just (Right Equality.HRefl) -> Just responseResult
              Just (Left _) -> Nothing
              Nothing -> Nothing
          _ -> Nothing

matchRequest
  :: forall (m :: LSP.Method LSP.FromServer LSP.Request).
     (LSP.Message m ~ LSP.RequestMessage m)
  => LSP.SMethod m
  -> LSP.FromServerMessage 
  -> Maybe (LSP.LspId m, LSP.MessageParams m)
matchRequest method message = case message of
          LSP.FromServerMess requestMethod request ->
            case method `LSP.mEqServer` requestMethod of
              Just (Right Equality.HRefl) -> Just $ case request of
                LSP.RequestMessage _ id _ parameters -> (id, parameters)
              Just (Left _) -> Nothing
              Nothing -> Nothing
          _ -> Nothing

matchNotification
  :: forall (m :: LSP.Method LSP.FromServer LSP.Notification).
     (LSP.Message m ~ LSP.NotificationMessage m)
  => LSP.SMethod m
  -> LSP.FromServerMessage 
  -> Maybe (LSP.MessageParams m)
matchNotification method message = case message of
          LSP.FromServerMess notificationMethod notification ->
            case method `LSP.mEqServer` notificationMethod of
              Just (Right Equality.HRefl) -> Just $ case notification of
                LSP.NotificationMessage _ _ parameters -> parameters
              Just (Left _) -> Nothing
              Nothing -> Nothing
          _ -> Nothing

sendRequest :: (Aeson.ToJSON (LSP.MessageParams m),
                  Aeson.FromJSON (LSP.SMethod m))
                  => LSP.SClientMethod m
                  -> LSP.MessageParams m
                  -> StateT LSPClient IO (LSP.LspId m)
sendRequest method parameters = do
  client@LSPClient{..} <- get
  let id = LSP.IdInt lspClientNextRequestID
  let message = LSP.RequestMessage "2.0" id method parameters

  sendEncodedMessage $ Aeson.encode message

  let requestMap' = fromJust $ LSP.updateRequestMap lspClientRequestMap id method
  put client { lspClientNextRequestID = lspClientNextRequestID + 1, lspClientRequestMap = requestMap' }
  return id

sendResponse :: (Aeson.ToJSON (LSP.ResponseResult m))
                  => LSP.LspId m
                  -> LSP.ResponseResult m
                  -> StateT LSPClient IO ()
sendResponse id result = sendResponseOrError id (Right result)

sendResponseOrError :: (Aeson.ToJSON (LSP.ResponseResult m))
                  => LSP.LspId m
                  -> Either LSP.ResponseError (LSP.ResponseResult m)
                  -> StateT LSPClient IO ()
sendResponseOrError id errorOrResult = do
  client@LSPClient{..} <- get
  let message = LSP.ResponseMessage "2.0" (Just id) errorOrResult
  sendEncodedMessage $ Aeson.encode message

sendNotification :: (Aeson.ToJSON (LSP.MessageParams m))
                 => LSP.SClientMethod (m :: LSP.Method LSP.FromClient LSP.Notification)
                 -> LSP.MessageParams m
                 -> StateT LSPClient IO ()
sendNotification method parameters = do
  let message = LSP.NotificationMessage "2.0" method parameters
  sendEncodedMessage $ Aeson.encode message

sendEncodedMessage :: BS.ByteString
                  -> StateT LSPClient IO ()
sendEncodedMessage encodedMessage = do
  LSPClient{..} <- get
  liftIO $ do
    case lspClientLogTraffic of
      Just handle -> do
        ANSI.hSetSGR handle [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Yellow]
        hPutStrLn handle $ "~~> send: " ++ BS.unpack encodedMessage
        ANSI.hSetSGR handle [ANSI.Reset]
      Nothing -> return ()

    BS.hPut lspClientServerStdin $ LSP.addHeader encodedMessage
    IO.hFlush lspClientServerStdin


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
