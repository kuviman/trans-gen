module Codegame.MessageGameModel where

import Prelude hiding (id)
import qualified Trans
import Trans (Trans)
import Codegame.ClientMessage (ClientMessage)
import Codegame.ServerMessage (ServerMessage)
import Data.Int

-- | Client message
data MessageGameModelClient = MessageGameModelClient {
    -- | Message
    message :: ClientMessage }
    deriving Show

instance Trans MessageGameModelClient where
    read = do
        message <- Trans.read
        return MessageGameModelClient {
            message }

    write MessageGameModelClient {
        message } = do
            Trans.write message

-- | Server message
data MessageGameModelServer = MessageGameModelServer {
    -- | Message
    message :: ServerMessage }
    deriving Show

instance Trans MessageGameModelServer where
    read = do
        message <- Trans.read
        return MessageGameModelServer {
            message }

    write MessageGameModelServer {
        message } = do
            Trans.write message

-- | Client or server message
data MessageGameModel
    -- | Client message
    = Client MessageGameModelClient
    -- | Server message
    | Server MessageGameModelServer
    deriving Show

instance Trans MessageGameModel where
    read = do
        tag :: Int32 <- Trans.read
        case tag of
            0 -> Client <$> Trans.read
            1 -> Server <$> Trans.read

    write (Client value) = do
        Trans.write (0 :: Int32)
        Trans.write value
    write (Server value) = do
        Trans.write (1 :: Int32)
        Trans.write value