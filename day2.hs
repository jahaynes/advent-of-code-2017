{-# LANGUAGE BangPatterns #-}

import qualified Data.ByteString.Char8 as C8
import           Data.Maybe                  (fromJust)
import           System.Environment          (getArgs)

data RowSummary = RowSummary !Int !Int

main :: IO ()
main = do

    [f] <- getArgs
    contents <- C8.readFile f

    let table = map (map (fst . fromJust . C8.readInt) . C8.words) . C8.lines $ contents

        summaries = map (foldl summarizeLine (RowSummary maxBound minBound)) table

    print . sum . map diff $ summaries

    where
    summarizeLine !(RowSummary lo hi) !next = RowSummary (min next lo) (max next hi)

    diff (RowSummary lo hi) = hi - lo
