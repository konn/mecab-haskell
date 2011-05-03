{-# LANGUAGE OverloadedStrings #-}
module Main where
import Text.MeCab
import Data.ByteString.Char8
import Prelude hiding (putStrLn, getLine) 
import Control.Monad

main :: IO ()
main = forever $ do
  wds <- mecab "" =<< getLine
  print wds --mapM_ (mapM_ (\w -> putStrLn $ word w `append` "\t" `append` description w)) wds
  return ()
  

