{-# LANGUAGE RecordWildCards #-}
module SyntheticWeb.Plan.Writer (writePlan) where

import qualified Data.ByteString.Char8 as BS
import Text.Printf (printf)
import SyntheticWeb.Plan.Types

writePlan :: Plan -> BS.ByteString
writePlan = BS.pack . unlines . concatMap writePattern

writePattern :: (Weight, Pattern) -> [String]
writePattern (Weight w, Pattern {..}) =
  [ printf "pattern %s with weight %d" name w
  ,        "[" ]                           ++
  commify (map writeActivity activities)   ++
  [        "]" ]

commify :: [String] -> [String]
commify []     = []
commify (x:xs) = x:map (\y -> ',':y) xs

writeActivity :: Activity -> String
writeActivity (SLEEP duration) = printf " SLEEP %s" (writeDuration duration)
writeActivity (GET flags size rate)  =
  printf " GET %s %s %s" (writeFlags flags) (writeSize size) (writeRate rate)
writeActivity (PUT flags size rate)  =
  printf " PUT %s %s %s" (writeFlags flags) (writeSize size) (writeRate rate)
writeActivity (Chatty flags size size' rate) =
  printf "Chatty %s %s %s %s" (writeFlags flags) (writeSize size)
                              (writeSize size') (writeRate rate)

writeDuration :: Duration -> String
writeDuration (Us duration) = printf "%d us" duration
writeDuration (Ms duration) = printf "%d ms" duration
writeDuration (S duration)  = printf "%d s" duration

writeSize :: Size -> String
writeSize (Exactly bytes) = printf "exactly %d" bytes
writeSize (Uniform range) = printf "uniform %d-%d" `uncurry` range
writeSize (Gauss range)   = printf "gauss %d-%d" `uncurry` range

writeRate :: Rate -> String
writeRate Unlimited        = "unlimited"
writeRate (LimitedTo size) = printf "limitedTo %s" (writeSize size)

writeFlags :: [Flag] -> String
writeFlags = show
