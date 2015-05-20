{-# LANGUAGE RecordWildCards #-}
module SyntheticWeb.Plan.Writer (writePlan) where

import Text.Printf (PrintfArg (), printf)
import SyntheticWeb.Plan.Types
import SyntheticWeb.Statistical (Statistical (..))

writePlan :: Plan -> String
writePlan (Plan plan) = go plan
    where go = unlines . concatMap writePattern

writePattern :: (Weight, Pattern) -> [String]
writePattern (Weight w, Pattern {..}) =
  printf "pattern %s with weight %d [" name w :
    replaceLastComma (map writeActivity activities)

replaceLastComma :: [String] -> [String]
replaceLastComma [] = ["]"]
replaceLastComma xs =
  let x:xs' = reverse xs
      x'    = uncommify x
  in reverse (x':xs')
  where
    uncommify = go . reverse
    go ""     = "]"
    go (_:ys) = reverse (']':ys)

writeActivity :: Activity -> String
writeActivity (SLEEP duration) = 
  printf " SLEEP %s," (writeDuration duration)
writeActivity (GET headers download rate) =
  printf " GET headers %s download %s rate %s," (show headers)
                                                (writeDownload download)
                                                (writeRate rate)
writeActivity (PUT headers upload) =
  printf " PUT headers %s upload %s," (show headers)
                                      (writeUpload upload)
writeActivity (POST headers upload download rate) =
  printf " POST headers %s upload %s download %s rate %s,"
         (show headers)
         (writeUpload upload)
         (writeDownload download)
         (writeRate rate)

writeDuration :: Duration -> String
writeDuration (Usec stat) = printf "%s usec" (writeStatistical stat)
writeDuration (Msec stat) = printf "%s msec" (writeStatistical stat)
writeDuration (Sec stat)  = printf "%s sec" (writeStatistical stat)

writeSize :: Size -> String
writeSize (Size stat) = printf "%s bytes" (writeStatistical stat)

writeStatistical :: PrintfArg a => Statistical a -> String
writeStatistical (Exactly bytes)  = printf "exactly %d" bytes
writeStatistical (Uniform range)  = printf "uniform %d-%d" `uncurry` range
writeStatistical (Gaussian range) = printf "gaussian %d,%d" `uncurry` range

writeDownload :: Download -> String
writeDownload (Download size) = writeSize size

writeUpload :: Upload -> String
writeUpload (Upload size) = writeSize size

writeRate :: Rate -> String
writeRate Unlimited        = "unlimited"
writeRate (LimitedTo size) = printf "limitedTo %s" (writeSize size)

