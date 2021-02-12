{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Control.Exception              as CE
import           Control.Monad
import           Data.Binary                    (Binary)
import qualified Data.Binary                    as Binary
import qualified Data.Binary.Get                as Binary
import qualified Data.Binary.Put                as Binary
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as BSL
import           Data.Maybe                     (catMaybes, isJust)
import           Network.Socket
import           Network.Socket.ByteString.Lazy as SBS
import qualified System.Environment
import qualified Trans

import Codegame.MessageGameModel

runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
    addr <- resolve
    CE.bracket (open addr) close client
    where
        resolve = do
            let hints = defaultHints { addrSocketType = Stream }
            head <$> getAddrInfo (Just hints) (Just host) (Just port)
        open addr = do
            sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
            connect sock $ addrAddress addr
            return sock

main :: IO ()
main = do
    args <- System.Environment.getArgs
    let host = args !! 0
    let port = args !! 1
    let stdout :: Bool = (args !! 2) == "true"
    runTCPClient host port $ \socket -> do
        contents <- SBS.getContents socket
        let models = readManyWithHasDataFlag Trans.read contents :: [MessageGameModel]
        forM_ models $ \model -> do
            when stdout $ print model
            SBS.sendAll socket (Binary.runPut $ Trans.write model)

-- | Parse a lazy 'BSL.ByteString' incrementally into a lazy list.
--
-- >>> take 10 (getManyIncremental Binary.getInt32host (BSL.cycle "1\NUL\NUL\NUL\NUL"))
-- [49,12544,3211264,822083584,0,49,12544,3211264,822083584,0]
getManyIncremental :: Binary.Get a -> BSL.ByteString -> [a]
getManyIncremental get = go getOne . BSL.toChunks
  where
    getOne = Binary.runGetIncremental get

    go (Binary.Fail _leftover _bytesRead err) _ = error err
    go (Binary.Done leftover _bytesRead x) chunks = x :
      if | null chunks && BS.null leftover -> []
         | BS.null leftover                -> go getOne chunks
         | otherwise                       -> go getOne (leftover : chunks)
    go decoder [] = go (Binary.pushEndOfInput decoder) []
    go decoder (chunk:chunks) = go (decoder `Binary.pushChunk` chunk) chunks

-- | Parse a value preceded by a boolean flag, indicating if there is data to be parsed.
--
-- >>> Binary.runGet (readWithHasDataFlag Binary.getInt32host) "\x00"
-- Nothing
-- >>> Binary.runGet (readWithHasDataFlag Binary.getInt32host) "\x01\xFF\xFF\xFF\xFF"
-- Just (-1)
-- >>> Binary.runGet (readWithHasDataFlag Binary.getInt32host) "\x00\xFF\xFF\xFF\xFF"
-- Nothing
readWithHasDataFlag :: Binary.Get a -> Binary.Get (Maybe a)
readWithHasDataFlag get = do
  hasData <- Trans.read
  if hasData
     then Just <$> get
     else return Nothing

-- | Parse a lazy 'BSL.ByteString' incrementally into a lazy list of values.
-- Each value is preceded by a boolean flag indicating that there is data.
--
-- >>> take 5 (readManyWithHasDataFlag Binary.getInt32host (BSL.cycle "\x01\xFF\xFF\xFF\xFF"))
-- [-1,-1,-1,-1,-1]
-- >>> readManyWithHasDataFlag Binary.getInt32host "\x01\xFF\xFF\xFF\xFF"
-- [-1]
-- >>> readManyWithHasDataFlag Binary.getInt32host "\x01\xFF\xFF\xFF\xFF\x00"
-- [-1]
-- >>> readManyWithHasDataFlag Binary.getInt32host "\x00\xFF\xFF\xFF\xFF\x00"
-- []
readManyWithHasDataFlag :: Binary.Get a -> BSL.ByteString -> [a]
readManyWithHasDataFlag get
  = catMaybes . takeWhile isJust . getManyIncremental (readWithHasDataFlag get)