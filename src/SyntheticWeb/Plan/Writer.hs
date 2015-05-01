{-# LANGUAGE RecordWildCards #-}
module SyntheticWeb.Plan.Writer (writePlan) where

import Text.Printf (printf)
import SyntheticWeb.Plan.Types

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
  printf " GET headers %s download %s rate %s," (writeHeaders headers)
                                                (writeDownload download)
                                                (writeRate rate)
writeActivity (PUT headers upload) =
  printf " PUT headers %s upload %s," (writeHeaders headers)
                                      (writeUpload upload)
writeActivity (POST headers upload download rate) =
  printf " POST headers %s upload %s download %s rate %s,"
         (writeHeaders headers)
         (writeUpload upload)
         (writeDownload download)
         (writeRate rate)

writeDuration :: Duration -> String
writeDuration (Usec duration) = printf "%d usec" duration
writeDuration (Msec duration) = printf "%d msec" duration
writeDuration (Sec duration)  = printf "%d sec" duration

writeSize :: Size -> String
writeSize (Exactly bytes) = printf "exactly %d" bytes
writeSize (Uniform range) = printf "uniform %d-%d" `uncurry` range
writeSize (Gauss range)   = printf "gauss %d-%d" `uncurry` range

writeDownload :: Download -> String
writeDownload (Download size) = writeSize size

writeUpload :: Upload -> String
writeUpload (Upload size) = writeSize size

writeRate :: Rate -> String
writeRate Unlimited        = "unlimited"
writeRate (LimitedTo size) = printf "limitedTo %s" (writeSize size)

writeHeaders :: [Header] -> String
writeHeaders = filter (/= '\"') . show . map toString
  where
    toString :: Header -> String
    toString AcceptAny              = "accept-any"
    toString AcceptTextHtml         = "accept-text-html"
    toString AcceptTextPlain        = "accept-text-plain"
    toString AcceptApplicationJSON  = "accept-application-json"
    toString ContentTextHtml        = "content-text-html"
    toString ContentTextPlain       = "content-text-plain"
    toString ContentApplicationJSON = "content-application-json"
