module Main where
  import Control.Monad
  import qualified Data.ByteString.Lazy as LBS
  import System.Environment
  import Text.PrettyPrint.GenericPretty

  import qualified Scanner
  import qualified Parser

  scan :: IO ()
  scan = do
    [inputFile] <- getArgs
    input <- LBS.readFile inputFile
    let result = Scanner.listTokens input
    case result of
      Left errorMessage -> putStrLn errorMessage
      Right tokens -> forM_ tokens $ putStrLn . show

  parse :: IO ()
  parse = do
    [inputFile] <- getArgs
    input <- LBS.readFile inputFile
    let result = Parser.runParser input
    case result of
      Left errorMessage -> putStrLn errorMessage
      Right ast -> pp ast

  main = scan
