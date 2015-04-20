{-# LANGUAGE RecordWildCards #-}
module SyntheticWeb.Plan.Writer (writePlan) where

import qualified Data.ByteString.Char8 as BS
import Text.Printf (printf)
import SyntheticWeb.Plan.Types

writePlan :: Plan -> BS.ByteString
writePlan (Plan plan) = go plan
    where go = BS.pack . unlines . concatMap writePattern

writePattern :: (Weight, Pattern) -> [String]
writePattern (Weight w, Pattern {..}) =
  [ printf "pattern %s with weight %d" name w
  ,        "[" ]                           ++
  commify (map (unlines . writeActivity) activities)   ++
  [        "]" ]

commify :: [String] -> [String]
commify []     = []
commify (x:xs) = x:map (\y -> ',':y) xs

writeActivity :: Activity -> [String]
writeActivity (SLEEP duration) = 
  [ printf " SLEEP %s" (writeDuration duration) ]
writeActivity (GET headers size rate) =
  [ printf " GET headers %s" (writeHeaders headers)
  , printf "     payload %s" (writePayload size)
  , printf "     rate %s" (writeRate rate) ]
writeActivity (PUT headers size rate) =
  [ printf " PUT headers %s" (writeHeaders headers)
  , printf "     payload %s" (writePayload size)
  , printf "     rate %s" (writeRate rate) ]

writeDuration :: Duration -> String
writeDuration (Us duration) = printf "%d us" duration
writeDuration (Ms duration) = printf "%d ms" duration
writeDuration (S duration)  = printf "%d s" duration

writeSize :: Size -> String
writeSize (Exactly bytes) = printf "exactly %d" bytes
writeSize (Uniform range) = printf "uniform %d-%d" `uncurry` range
writeSize (Gauss range)   = printf "gauss %d-%d" `uncurry` range

writePayload :: Payload -> String
writePayload (Payload size) = writeSize size

writeRate :: Rate -> String
writeRate Unlimited        = "unlimited"
writeRate (LimitedTo size) = printf "limitedTo %s" (writeSize size)

writeHeaders :: [Header] -> String
writeHeaders = show
