module Utilities
(
    getName,
    decode
)
where

import System.IO (hFlush, stdout)
import Data.Binary (Binary, decodeOrFail)
import Data.Binary.Get (ByteOffset)
import Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)

getName :: IO String
getName = do
    putStr "Enter your name: "
    hFlush stdout
    getLine

decode :: (Binary a) => BS.ByteString -> 
                        Either (BL.ByteString, ByteOffset, String)
                               (BL.ByteString, ByteOffset, a)
decode msg = decodeOrFail $ fromStrict msg
