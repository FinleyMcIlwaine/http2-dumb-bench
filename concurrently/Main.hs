module Main where
import UnliftIO.Async
import Control.Monad (when)

main :: IO ()
main = do
  concurrently_ countUp countDown

countTo :: Integer
countTo = 1000000

countUp :: IO ()
countUp =
    go 0
  where
    go n
      | n < countTo
      = do
        when (n `mod` 100000 == 0) $
          putStrLn $ "countUp at " ++ show n
        go (n + 1)
      | otherwise
      = putStrLn "countUp done"

countDown :: IO ()
countDown =
    go countTo
  where
    go n
      | n > 0
      = do
        when (n `mod` 100000 == 0) $
          putStrLn $ "countDown at " ++ show n
        go (n - 1)
      | otherwise
      = putStrLn "countDown done"
