-- Copyright Luke Lau 2018-2020.
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--
--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials provided
--       with the distribution.
--
--     * Neither the name of Luke Lau nor the names of other
--       contributors may be used to endorse or promote products derived
--       from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeInType #-}
module LSPDecode where

import           Prelude                 hiding ( id )
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Foldable
import           Data.Functor.Product
import           Data.Functor.Const
import           Control.Exception
import           Control.Lens
import qualified Data.ByteString.Lazy.Char8    as B
import           Data.Maybe
import           System.IO
import           System.IO.Error
import           Language.LSP.Types
import           Language.LSP.Types.Lens
import           Language.LSP.Test (SessionException(NoContentLengthHeader, UnexpectedServerTermination))

import Data.IxMap
import Data.Kind

-- | Fetches the next message bytes based on
-- the Content-Length header
getNextMessage :: Handle -> IO B.ByteString
getNextMessage h = do
  headers <- getHeaders h
  case read . init <$> lookup "Content-Length" headers of
    Nothing   -> throw NoContentLengthHeader
    Just size -> B.hGet h size

addHeader :: B.ByteString -> B.ByteString
addHeader content = B.concat
  [ "Content-Length: "
  , B.pack $ show $ B.length content
  , "\r\n"
  , "\r\n"
  , content
  ]

getHeaders :: Handle -> IO [(String, String)]
getHeaders h = do
  l <- catch (hGetLine h) eofHandler
  let (name, val) = span (/= ':') l
  if null val then return [] else ((name, drop 2 val) :) <$> getHeaders h
  where eofHandler e
          | isEOFError e = throw UnexpectedServerTermination
          | otherwise = throw e

type RequestMap = IxMap LspId (SMethod :: Method FromClient Request -> Type )

newRequestMap :: RequestMap
newRequestMap = emptyIxMap

updateRequestMap :: RequestMap -> LspId m -> SClientMethod m -> Maybe RequestMap
updateRequestMap reqMap id method = insertIxMap id method reqMap

getRequestMap :: [FromClientMessage] -> RequestMap
getRequestMap = foldl' helper emptyIxMap
 where
  helper :: RequestMap -> FromClientMessage -> RequestMap
  helper acc msg = case msg of
    FromClientMess m mess -> case splitClientMethod m of
      IsClientNot -> acc
      IsClientReq -> fromJust $ updateRequestMap acc (mess ^. id) m
      IsClientEither -> case mess of
        NotMess _ -> acc
        ReqMess msg -> fromJust $ updateRequestMap acc (msg ^. id) m
    _ -> acc

decodeFromServerMsg :: RequestMap -> B.ByteString -> (RequestMap, FromServerMessage)
decodeFromServerMsg reqMap bytes = unP $ parse p obj
  where obj = fromJust $ decode bytes :: Value
        p = parseServerMessage $ \lid ->
          let (mm, newMap) = pickFromIxMap lid reqMap
            in case mm of
              Nothing -> Nothing
              Just m -> Just $ (m, Pair m (Const newMap))
        unP (Success (FromServerMess m msg)) = (reqMap, FromServerMess m msg)
        unP (Success (FromServerRsp (Pair m (Const newMap)) msg)) = (newMap, FromServerRsp m msg)
        unP (Error e) = error e
