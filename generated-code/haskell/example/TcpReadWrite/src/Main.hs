import qualified Trans
import Control.Monad
import qualified Data.Binary as Bin
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import qualified Data.ByteString.Lazy as BS
import qualified System.Environment
import qualified Control.Exception as CE
import Network.Socket
import Network.Socket.ByteString.Lazy as SBS
import Example

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
    let stdout :: Bool = read $ args !! 2
    runTCPClient host port $ \socket -> do
        contents <- SBS.getContents socket
        let models = runGet readModels contents
        forM_ models $ \model -> do
            when stdout $ print model
            SBS.sendAll socket (runPut $ Trans.write model)
        where
            readModels :: Bin.Get [Example]
            readModels = do
                hasData :: Bool <- Trans.read
                if hasData then do
                    x <- Trans.read
                    xs <- readModels
                    return (x:xs)
                else
                    return []