module Main where
  import Control.Monad
  import qualified Data.ByteString.Lazy as LBS
  import System.Environment
  import Text.PrettyPrint.GenericPretty

  import qualified Parser

  parse = do
    [inputFile] <- getArgs
    input <- LBS.readFile inputFile
    let parseTree = Parser.runParser input
    pp parseTree

  main = parse
