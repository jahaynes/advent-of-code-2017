{-# LANGUAGE BangPatterns #-}

import           System.Environment                 (getArgs)
import           Data.Word                          (Word8)
import           Data.Vector.Storable               (Vector, (!))
import qualified Data.Vector.Storable       as VS
import           Data.Vector.Storable.MMap 

data Part = One | Two 

main :: IO ()
main = do

    [f] <- getArgs
    contents <- unsafeMMapVector f Nothing

    let digits = VS.filter ( < 10) 
               . VS.map (\x -> x - 48)
               $ contents

    let len = VS.length digits
               
    print $ part One digits len

    print $ part Two digits len

    where
    part :: Part -> Vector Word8 -> Int -> Int
    part p !vec !len = go 0 0 
    
        where
        go :: Int -> Int -> Int
        go !acc !i

            | i == len = acc

            | otherwise =
                if vec ! i == vec ! k
                   then go (acc + fromIntegral (vec ! i)) (i + 1)
                   else go                            acc (i + 1)

            where
            k = case p of 
                    One -> (i +             1) `mod` len
                    Two -> (i + (len `div` 2)) `mod` len
