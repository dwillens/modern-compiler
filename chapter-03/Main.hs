module Main where
  import Control.Monad
  import qualified Data.ByteString.Lazy as LBS
  import System.Environment

  import Scanner
  import Parser

  scan = do
    [inputFile] <- getArgs
    input <- LBS.readFile inputFile
    let tokens = scanTokens input
    forM tokens $ putStrLn . show

  parse = do
    [inputFile] <- getArgs
    input <- LBS.readFile inputFile
    let tokens = scanTokens input
    forM tokens $ putStrLn . show
    let parseTree = makeParseTree tokens
    putStrLn $ show parseTree
  main = parse
