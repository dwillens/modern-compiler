module Main where
  import Control.Monad
  import qualified Data.ByteString.Lazy as L
  import System.Environment
  import System.IO
  import Text.PrettyPrint.GenericPretty

  import qualified Scanner
  import qualified Parser
  import qualified TypeChecker

  showScan :: L.ByteString -> IO ()
  showScan input =
    case Scanner.listTokens input of
      Left errorMessage -> putStrLn errorMessage
      Right tokens -> forM_ tokens $ putStrLn . show

  showParse :: L.ByteString -> IO ()
  showParse input =
    case Parser.runParser input of
      Left errorMessage -> putStrLn errorMessage
      Right ast -> pp ast

  typeCheck :: L.ByteString -> IO ()
  typeCheck input =
    case Parser.runParser input >>= TypeChecker.typeCheck of
      Left errorMessage -> putStrLn errorMessage
      Right () -> putStrLn "Types correct"

  main = do
    [phase, inputFile] <- getArgs
    input <- case inputFile of
                  "-" -> L.hGetContents stdin
                  otherwise -> L.readFile inputFile
    case phase of
        "scan" -> showScan input
        "parse" -> showParse input
        "typeCheck" -> typeCheck input
