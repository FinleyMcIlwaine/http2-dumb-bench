{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent
import Control.Monad
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C

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
client n = do
    runTCPClient "localhost" "3000" . loop
  where
    loop 0 s = sendAll s "" >> putStrLn "done"
    loop n s = do
      let out = if n == 0 then "" else C.pack $ show n
      sendAll s out
      msg <- recv s
      when (n `mod` 1000 == 0) $ do
        putStr "Received: "
        C.putStrLn msg
      loop (n - 1) s
