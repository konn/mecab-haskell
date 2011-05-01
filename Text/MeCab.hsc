#include "mecab_wrap.h"
#include <mecab.h>
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
{-# LANGUAGE RecordWildCards #-}
module Text.MeCab where
import Foreign
import Foreign.Ptr
import Foreign.C
import Foreign.Storable
import Data.Word
import Data.ByteString.Char8 hiding (elem)
import Control.Applicative

data MeCabNode = MeCabNode { prev  :: Ptr MeCabNode
                           , next  :: Ptr MeCabNode
                           , enext :: Ptr MeCabNode
                           , bnext :: Ptr MeCabNode
                           , surface :: CString
                           , feature :: CString
                           , len :: CUInt
                           , rlen :: CUInt
                           , identifier :: CUInt
                           , rcAttr :: CUShort
                           , lcAttr :: CUShort
                           , posid :: CUShort
                           , charType :: CUChar
                           , stat :: CUChar
                           , best :: CUChar
                           , alpha :: CFloat
                           , beta :: CFloat
                           , prob :: CFloat
                           , wcost :: CShort
                           , cost  :: CLong
                           } deriving (Show, Eq, Ord)

data MeCab

data Status = Normal
            | Unknown
            | BOS
            | EOS
            | Other Int
              deriving (Eq, Ord, Show, Read)

data Node = Node { word         :: ByteString
                 , description  :: ByteString
                 , uniqID       :: Word
                 , rightAttr    :: Int
                 , leftAttr     :: Int
                 , partOfSpeech :: Int
                 , character    :: Int
                 , status       :: Status
                 , isBest       :: Bool
                 , alphaProb    :: Float
                 , betaProb     :: Float
                 , probability  :: Float
                 , wordCost     :: Int
                 , totalCost    :: Word
                 } deriving (Show, Eq, Ord, Read)

mecab :: ByteString -> IO [Node]
mecab str = do
  mec <- withCString "-l1 -Ochasen" c_mecab_new2
  ans <- toNodes =<< str `useAsCString` c_mecab mec
  c_mecab_destroy mec
  return ans

toStatus :: CUChar -> Status
toStatus 0 = Normal
toStatus 1 = Unknown
toStatus 2 = BOS
toStatus 3 = EOS
toStatus n = Other (fromIntegral n)

toNodes :: Ptr MeCabNode -> IO [Node]
toNodes ptr
  | ptr == nullPtr = return []
  | otherwise      = do
      mnode <- peek ptr
      case stat mnode of
        2 -> toNodes (next mnode)
        3 -> return []
        _ -> do
          leng <- c_len ptr
          wd <- packCStringLen (surface mnode, fromIntegral leng)
          desc <- packCString (feature mnode)
          let node = Node { word = wd
                          , description = desc
                          , uniqID = fromIntegral $ identifier mnode
                          , rightAttr = fromIntegral $ rcAttr mnode
                          , leftAttr  = fromIntegral $ lcAttr mnode
                          , partOfSpeech = fromIntegral $ posid mnode
                          , character = fromIntegral $ charType mnode
                          , status = toStatus (stat mnode)
                          , isBest = best mnode == 1
                          , alphaProb = realToFrac $ alpha mnode
                          , betaProb = realToFrac $ beta mnode
                          , probability = realToFrac $ prob mnode
                          , wordCost    = fromIntegral $ wcost mnode
                          , totalCost   = fromIntegral $ cost mnode
                          }
          (node:) <$> toNodes (next mnode)

foreign import ccall "mecab_new2" c_mecab_new2 :: CString -> IO (Ptr MeCab)
foreign import ccall "mecab_destroy" c_mecab_destroy :: Ptr MeCab -> IO ()
foreign import ccall "mecab_sparse_tonode" c_mecab :: Ptr MeCab -> CString -> IO (Ptr MeCabNode)
foreign import ccall "mecab_wrap.h next_node" c_next :: Ptr MeCabNode -> IO (Ptr MeCabNode)
foreign import ccall "mecab_wrap.h prev_node" c_prev :: Ptr MeCabNode -> IO (Ptr MeCabNode)
foreign import ccall "mecab_wrap.h enext_node" c_enext :: Ptr MeCabNode -> IO (Ptr MeCabNode)
foreign import ccall "mecab_wrap.h bnext_node" c_bnext :: Ptr MeCabNode -> IO (Ptr MeCabNode)
foreign import ccall "mecab_wrap.h surface" c_surface ::  Ptr MeCabNode -> IO CString
foreign import ccall "mecab_wrap.h feature" c_feature ::  Ptr MeCabNode -> IO CString
foreign import ccall "mecab_wrap.h length" c_len ::  Ptr MeCabNode -> IO Word
foreign import ccall "mecab_wrap.h rlength" c_rlen ::  Ptr MeCabNode -> IO Word
foreign import ccall "mecab_wrap.h id" c_identifier ::  Ptr MeCabNode -> IO Word
foreign import ccall "mecab_wrap.h rcAttr" c_rcAttr ::  Ptr MeCabNode -> IO Word16
foreign import ccall "mecab_wrap.h lcAttr" c_lcAttr ::  Ptr MeCabNode -> IO Word16
foreign import ccall "mecab_wrap.h posid" c_posid ::  Ptr MeCabNode -> IO Word16
foreign import ccall "mecab_wrap.h char_type" c_charType ::  Ptr MeCabNode -> IO CUChar
foreign import ccall "mecab_wrap.h stat" c_stat ::  Ptr MeCabNode -> IO CUChar
foreign import ccall "mecab_wrap.h isbest" c_best ::  Ptr MeCabNode -> IO Bool
foreign import ccall "mecab_wrap.h alpha" c_alpha ::  Ptr MeCabNode -> IO Float
foreign import ccall "mecab_wrap.h beta" c_beta ::  Ptr MeCabNode -> IO Float
foreign import ccall "mecab_wrap.h wcost" c_wcost ::  Ptr MeCabNode -> IO Word16
foreign import ccall "mecab_wrap.h cost" c_cost ::  Ptr MeCabNode -> IO Word

instance Storable MeCabNode where
  sizeOf = const #size struct mecab_node_t
  alignment _ = #{alignment mecab_node_t}
  poke pt node = do
    (#poke struct mecab_node_t, next) pt (next node)
    (#poke struct mecab_node_t, prev) pt (prev node)
    (#poke struct mecab_node_t, enext) pt (enext node)
    (#poke struct mecab_node_t, bnext) pt (bnext node)
    (#poke struct mecab_node_t, surface) pt (surface node)
    (#poke struct mecab_node_t, feature) pt (feature node)
    (#poke struct mecab_node_t, length) pt (len node)
    (#poke struct mecab_node_t, rlength) pt (rlen node)
    (#poke struct mecab_node_t, id) pt (identifier node)
    (#poke struct mecab_node_t, rcAttr) pt (rcAttr node)
    (#poke struct mecab_node_t, lcAttr) pt (lcAttr node)
    (#poke struct mecab_node_t, posid) pt (posid node)
    (#poke struct mecab_node_t, char_type) pt (charType node)
    (#poke struct mecab_node_t, stat) pt (stat node)
    (#poke struct mecab_node_t, isbest) pt (best node)
    (#poke struct mecab_node_t, alpha) pt (alpha node)
    (#poke struct mecab_node_t, beta) pt (beta node)
    (#poke struct mecab_node_t, prob) pt (prob node)
    (#poke struct mecab_node_t, wcost) pt (wcost node)
    (#poke struct mecab_node_t, cost) pt (cost node)
  peek pt = do
    next <- (#peek struct mecab_node_t, next) pt
    prev <- (#peek struct mecab_node_t, prev) pt
    enext <- (#peek struct mecab_node_t, enext) pt
    bnext <- (#peek struct mecab_node_t, bnext) pt
    surface <- (#peek struct mecab_node_t, surface) pt
    feature <- (#peek struct mecab_node_t, feature) pt
    len <- (#peek struct mecab_node_t, length) pt
    rlen <- (#peek struct mecab_node_t, rlength) pt
    identifier <- (#peek struct mecab_node_t, id) pt
    rcAttr <- (#peek struct mecab_node_t, rcAttr) pt
    lcAttr <- (#peek struct mecab_node_t, lcAttr) pt
    posid <- (#peek struct mecab_node_t, posid) pt
    charType <- (#peek struct mecab_node_t, char_type) pt
    stat <- (#peek struct mecab_node_t, stat) pt
    best <- (#peek struct mecab_node_t, isbest) pt
    alpha <- (#peek struct mecab_node_t, alpha) pt
    beta <- (#peek struct mecab_node_t, beta) pt
    prob <- (#peek struct mecab_node_t, prob) pt
    wcost <- (#peek struct mecab_node_t, wcost) pt
    cost <- (#peek struct mecab_node_t, cost) pt
    return MeCabNode{..}
