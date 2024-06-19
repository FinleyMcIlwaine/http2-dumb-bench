{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent
import Control.Monad
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import UnliftIO.Async

-- network-run
import Network.Run.TCP (runTCPServer, runTCPClient)

-- network
import Network.Socket.ByteString (recv, sendAll)

main :: IO ()
main = do
    _ <- forkIO server
    threadDelay 10_000
    client 10000

server :: IO ()
server = runTCPServer Nothing "3000" talk
  where
    talk s = do
        msg <- recv s 1024
        unless (S.null msg) $ do
          sendAll s msg
          talk s

client :: Int -> IO ()
client n =
    runTCPClient "localhost" "3000" $ \sock -> do
      ((),()) <- concurrently (loop n sock) (loop (n * 2) sock)
      sendAll sock ""
  where
    loop 0 _ = putStrLn "done"
    loop m s = do
      let out = if m == 0 then "" else C.pack $ show m
      sendAll s out
      msg <- recv s 1024
      when (m `mod` 1000 == 0) $ do
        putStr "Received: "
        C.putStrLn msg
      loop (m - 1) s
