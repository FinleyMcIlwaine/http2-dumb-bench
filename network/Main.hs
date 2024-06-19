{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent
import Control.Monad (when)
import Data.IORef
import System.IO.Unsafe
import UnliftIO.Async
import UnliftIO.STM

-- network-run
import Network.Run.TCP (runTCPServer, runTCPClient)

-- network
import Network.Socket.ByteString (recv, sendAll)
import Network.Socket (Socket)

main :: IO ()
main = do
    _ <- forkIO server
    threadDelay 10_000
    client

server :: IO ()
server = runTCPServer Nothing "3000" (talk 0)
  where
    talk :: Int -> Socket -> IO ()
    talk n s
      | n > 1000000
      = return ()
      | otherwise
      = do
        _ <- recv s 1
        sendAll s "0"
        talk (n + 1) s

client :: IO ()
client =
    runTCPClient "localhost" "3000" $ \sock -> do
      race (runBackgroundThreads sock) (runClient sock) >> return ()
  where
    runBackgroundThreads s = concurrently_ (frameReceiver s) (frameSender s)

runClient :: Socket -> IO ()
runClient sock = do
    sendAll sock "0"
    receive sock
    recvd <- readIORef numBs
    when (recvd `mod` 1000 == 0) $ putStrLn $ "recvd: " ++ show recvd
    if recvd > 100000 then
      return ()
    else
      runClient sock

frameReceiver :: Socket -> IO ()
frameReceiver sock = loop 0
  where
    loop :: Int -> IO ()
    loop n
        | n == 6 = do
            yield
            loop 0
        | otherwise = do
            receive sock
            if even n
                then enqueueControl controlQ ()
                else do
                    loop (n + 1)

frameSender :: Socket -> IO ()
frameSender sock = loop
  where
    loop :: IO ()
    loop = do
      x <- atomically $ readTQueue controlQ
      case x of
          () -> sendAll sock "0" >> loop

receive :: Socket -> IO ()
receive sock = do
    "0" <- recv sock 1
    atomicModifyIORef' numBs (\n -> (n + 1, ()))

{-# NOINLINE numBs #-}
numBs :: IORef Int
numBs = unsafePerformIO $ newIORef 0

{-# NOINLINE controlQ #-}
controlQ :: TQueue ()
controlQ = unsafePerformIO $ newTQueueIO

enqueueControl :: TQueue () -> () -> IO ()
enqueueControl ctlQ ctl = atomically $ writeTQueue ctlQ ctl
