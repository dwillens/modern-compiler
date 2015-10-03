module Main where
  import Control.Monad
  import qualified Data.ByteString.Lazy as LBS
  import System.Environment

  import Scanner

  scan = do
    [inputFile, outputFile] <- getArgs
    input <- LBS.readFile inputFile
    let tokens = scanTokens input
    forM tokens $ putStrLn . show

  main = scan
