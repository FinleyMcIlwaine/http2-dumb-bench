{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Main where

import Control.Concurrent
import Control.Monad
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as C8
import Data.String
import qualified UnliftIO.Exception as E


-- network-run
import Network.HTTP.Types -- network-run
import Network.Run.TCP (runTCPServer, runTCPClient) -- network-run

-- http2
import Network.HTTP2.Client as Client
import Network.HTTP2.Server as Server
import UnliftIO (concurrently_)


main :: IO ()
main = do
    _ <- forkIO myServer
    threadDelay 10_000
    runClient 400


myServer :: IO ()
myServer =
    runTCPServer (Just serverName) "12080" runHTTP2Server
  where
    runHTTP2Server s =
        allocSimpleConfig s 4096 `E.bracket` freeSimpleConfig $
          \conf ->
            Server.run
              defaultServerConfig
              conf
              server

    server _req _aux sendResponse = sendResponse response []
      where
        response = responseBuilder ok200 header body
        header = [(fromString "Content-Type", C8.pack "text/plain")] :: ResponseHeaders
        body = BSB.string8 "Hello, world!\n"

serverName :: String
serverName = "localhost"

runClient :: Int -> IO ()
runClient requests =
    runTCPClient serverName "12080" runHTTP2Client
  where
    runHTTP2Client s =
        allocSimpleConfig s 4096 `E.bracket` freeSimpleConfig $
          \conf ->
            Client.run
              (defaultClientConfig { authority = serverName })
              conf
              client

    client :: Client ()
    client sendRequest _aux =
        forM_ [0..requests :: Int] $ \i -> do
          when (i `mod` 50 == 0) $ print i
          let
            req0 = requestNoBody methodGet (C8.pack "/") []
            client0 = sendRequest req0 $ \rsp -> do
                -- print rsp
                !_r <- getResponseBodyChunk rsp :: IO C8.ByteString
                return ()
                -- C8.putStrLn r
            req1 = requestNoBody methodGet (C8.pack "/foo") []
            client1 = sendRequest req1 $ \rsp -> do
                -- print rsp
                !_r <- getResponseBodyChunk rsp
                return ()
          concurrently_ client0 client1
