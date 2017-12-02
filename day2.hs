{-# LANGUAGE BangPatterns #-}

import qualified Data.ByteString.Char8 as C8
import           Data.Maybe                  (fromJust)
import           Data.Vector.Unboxed         ((!))
import qualified Data.Vector.Unboxed as VU
import           System.Environment          (getArgs)

data RowSummary = RowSummary !Int !Int

main :: IO ()
main = do

    [f] <- getArgs
    contents <- C8.readFile f

    let table = map VU.fromList
              . map (map (fst . fromJust . C8.readInt) . C8.words)
              . C8.lines
              $ contents

    print (partOne table)

    print (partTwo table)

    where

    partOne = sum . map diff . map (VU.foldl summarizeLine (RowSummary maxBound minBound))

        where
        summarizeLine !(RowSummary lo hi) !next = RowSummary (min next lo) (max next hi)

        diff (RowSummary lo hi) = hi - lo

    partTwo = sum . map (\row -> scanRow row (VU.length row))

        where
        scanRow !row !len = go 0 1 

            where
            go !i !j | i == len  = error "no solution"
                     | j == len  = go (i+1) (i+2)
                     | otherwise =
                         case a `divMod` b of
                             (n, 0) -> n
                             _      -> go i (j+1)
                where
                a = max (row ! i) (row ! j)
                b = min (row ! i) (row ! j)

