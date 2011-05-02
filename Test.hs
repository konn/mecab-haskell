{-# LANGUAGE OverloadedStrings #-}
module Main where
import Text.MeCab
import Data.ByteString.Char8
import Prelude hiding (getLine, putStrLn)
import Control.Monad

main :: IO ()
main = forever $ do
  wds <- mecab =<< getLine
  mapM (\w -> putStrLn $ word w `append` "\t" `append` description w) wds
  return ()
  

