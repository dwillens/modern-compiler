module Main where
  import Scanner

  main = forM Scanner.scan $ putStrLn . show