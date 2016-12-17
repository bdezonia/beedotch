{-

Beedotch

MIT License

Copyright (c) 2016 Barry DeZonia

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

-}

{-# LANGUAGE FlexibleContexts #-} -- for casting code

import Data.Array.ST (newArray, readArray, MArray, STUArray)
import Data.Array.Unsafe (castSTUArray)
import GHC.ST (runST, ST)

import Data.Bool
import Data.Int
import Data.Word
import Data.List
import Data.Monoid
import System.IO

type Metadata = [String]

type Rows = [[String]]

data Numbers = Bytes [Word8] |
               Shorts [Int16] |
               Ints [Int32] |
               Longs [Int64] |
               Floats [Float] |
               Doubles [Double]

data FitsData = PrimaryData Metadata Numbers |
                Image Metadata Numbers |
                AsciiTable Metadata Rows |
                BinTable Metadata Rows
  
type FitsFile = [FitsData]

{-  Getting endina words info from Data.Binary

http://hackage.haskell.org/package/binary-0.7.5.0/docs/Data-Binary-Get.html

-}

{-
  casting
  ideas from:
  http://stackoverflow.com/questions/6976684/converting-ieee-754-floating-point-in-haskell-word32-64-to-and-from-haskell-floa
-}

wordToByte :: Word8 -> Word8
wordToByte = id

wordToShort :: Word16 -> Int16
wordToShort x = runST (cast x)

wordToInt :: Word32 -> Int32
wordToInt x = runST (cast x)

wordToLong :: Word64 -> Int64
wordToLong x = runST (cast x)

wordToFloat :: Word32 -> Float
wordToFloat x = runST (cast x)

wordToDouble :: Word64 -> Double
wordToDouble x = runST (cast x)

byteToWord :: Word8 -> Word8
byteToWord = id

shortToWord :: Int16 -> Word16
shortToWord x = runST (cast x)

intToWord :: Int32 -> Word32
intToWord x = runST (cast x)

longToWord :: Int64 -> Word64
longToWord x = runST (cast x)

floatToWord :: Float -> Word32
floatToWord x = runST (cast x)

doubleToWord :: Double -> Word64
doubleToWord x = runST (cast x)

{-# INLINE cast #-}
cast :: (MArray (STUArray s) a (ST s),
         MArray (STUArray s) b (ST s)) => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0

--withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
--openBinaryFile :: FilePath -> IOMode -> IO Handle
--hGetChar :: Handle -> IO Char
--hGetBuf :: Handle -> Ptr a -> Int -> IO Int
--hClose :: Handle -> IO ()
--hIsEOF :: Handle -> IO Bool

--ReadMode	 
--WriteMode	 
--AppendMode	 
--ReadWriteMode

--FilePath is just a String

readHeader :: Handle -> IO Metadata
readHeader handle = readChunks handle []

readChunks :: Handle -> [String] -> IO [String]
readChunks handle accum = do
  chunk <- readHeaderChunk handle
  if isLast chunk
    then return (accum ++ chunk)
    else readChunks handle (accum ++ chunk)

isLast :: [String] -> Bool
isLast chunkStrings = case (find isEnd chunkStrings) of
  Just _ -> True
  Nothing -> False

isEnd :: String -> Bool
isEnd string = isPrefixOf "END     " string

readHeaderChunk :: Handle -> IO [String]
readHeaderChunk handle = readlines 36 handle

readlines :: Int -> Handle -> IO [String]
readlines 0 handle = return []
readlines n handle = do
  line <- readN 80 handle
  lines <- readlines (n-1) handle
  return (line:lines)

readN :: Integer -> Handle -> IO String
readN counter handle = do
  if counter == 0
    then return ""
    else do
      char <- hGetChar handle
      chars <- readN (counter-1) handle
      return (char:chars)

repl :: Num a => Integer -> a -> [a]
repl 0 _ = []
repl n v = v : repl (n-1) v

-- TODO finish me
readBytes :: Handle -> Integer -> IO Numbers
readBytes h count = do
  readN (1*count) h
  return (Bytes (repl count 0))

-- TODO finish me
readShorts :: Handle -> Integer -> IO Numbers
readShorts h count = do
  readN (2*count) h
  return (Shorts (repl count 0))

-- TODO finish me
readInts :: Handle -> Integer -> IO Numbers
readInts h count = do
  readN (4*count) h
  return (Ints (repl count 0))

-- TODO finish me
readLongs :: Handle -> Integer -> IO Numbers
readLongs h count = do
  readN (8*count) h
  return (Longs (repl count 0))

-- TODO finish me
readFloats :: Handle -> Integer -> IO Numbers
readFloats h count = do
  readN (4*count) h
  return (Floats (repl count 0))

-- TODO finish me
readDoubles :: Handle -> Integer -> IO Numbers
readDoubles h count = do
  readN (8*count) h
  return (Doubles (repl count 0))

readNumbers :: Handle -> Metadata -> IO Numbers
readNumbers handle metadata =
    case (bitpix metadata) of
      (  8) -> readBytes   handle count 
      ( 16) -> readShorts  handle count
      ( 32) -> readInts    handle count
      ( 64) -> readLongs   handle count 
      (-32) -> readFloats  handle count
      (-64) -> readDoubles handle count
    where
      count = sampleCount (dimensions metadata)

bitpix :: Metadata -> Integer
bitpix metadata = read (get metadata "BITPIX" "0")

dimensions :: Metadata -> [Integer]
dimensions metadata = dimensions' 0 (axisCount metadata) []
  where
    dimensions' i maxPlusOne accum
      | i == maxPlusOne = reverse accum
      | otherwise = dimensions' (i+1) maxPlusOne ((axisSize metadata (i+1)): accum)

sampleCount :: [Integer] -> Integer
sampleCount dims = foldl (*) 1 dims

axisCount :: Metadata -> Integer
axisCount metadata = read (get metadata "NAXIS" "0")

axisSize :: Metadata -> Integer -> Integer
axisSize metadata axisNumber = read (get metadata (anum axisNumber) "0")
  where anum i = "NAXIS" ++ (show i)

get :: Metadata -> String -> String -> String
get [] _ defaultValue = defaultValue
get (x:xs) fieldName defaultValue =
  case (match fieldName x) of
    Just st -> stripComment (secondPart st)
    Nothing -> get xs fieldName defaultValue
     
match :: String -> String -> Maybe String
match fieldName headerString =
  if (isPrefixOf fieldName headerString)
    then (Just headerString)
    else Nothing

secondPart :: String -> String
secondPart st = drop 10 st

stripComment :: String -> String
stripComment st = takeWhile isNotCommentChar st

isNotCommentChar :: Char -> Bool
isNotCommentChar '/' = False
isNotCommentChar _ = True

totalDataBytes :: Metadata -> Integer
totalDataBytes metadata = (abs (bitpix metadata)) * (sampleCount (dimensions metadata)) `div` 8

alignToNewFrame :: Handle -> Metadata -> IO ()
alignToNewFrame handle metadata = do
  let count = 2880 - ((totalDataBytes metadata) `mod` 2880) in
    readN count handle
  return ()

-- TODO loop until eof

main = do
  h <- openBinaryFile "file.fits" ReadMode
  header <- readHeader h
  numbers <- readNumbers h header
  alignToNewFrame h header
  isEOF <- hIsEOF h
  hClose h
  mapM_ putStrLn header
  putStrLn ("At eof : " ++ (show isEOF))
  --putStrLn ("" ++ (show (sampleCount [100,100])))
