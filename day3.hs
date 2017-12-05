{-# LANGUAGE BangPatterns #-}

import qualified Data.HashTable.IO as H
import Data.Maybe (fromMaybe)

main :: IO ()
main = do

    let input = 361527

    print $ partOne input

    print =<< partTwo input

partOne :: Int -> Int
partOne input =

    let lengths = concatMap (\x -> [x,x]) [1..]

        directionsX = cycle [1, 0,-1, 0]

        directionsY = cycle [0, 1, 0,-1]

    in bigSteps 1 0 0 lengths directionsX directionsY

    where
    bigSteps !bn !box !boy (len:lens) (dx:dxs) (dy:dys) =
        
        let box' = box + len * dx
            boy' = boy + len * dy
            dist = abs (len * dx) + abs (len * dy)
            bn' = bn + dist
        in
        if bn' >= input
            then smallSteps bn box boy
            else bigSteps bn' box' boy' lens dxs dys

        where
        smallSteps sn !sox !soy
            | sn == input = abs sox + abs soy
            | otherwise   = smallSteps (sn+1) (sox + dx) (soy + dy)

type Grid = H.BasicHashTable (Int, Int) Int



partTwo :: Int -> IO Int
partTwo input = do

    let directions = cycle [ ( 1, 0)
                           , ( 0, 1)
                           , (-1, 0)
                           , ( 0,-1) ]

        lengths = concatMap (\x -> [x, x]) [1..]

        steps = concat . zipWith replicate lengths $ directions

    grid <- H.new :: IO Grid
    H.insert grid (0, 0) 1
    go grid (0,0) steps
    where
    go grid (px,py) ((dx,dy):steps) = do

        let px' = px + dx
            py' = py + dy

        s <- sumNeighbours grid (px',py')

        if s > input
            then return s
            else do
                H.insert grid (px',py') s
                go grid (px',py') steps

sumNeighbours :: Grid -> (Int, Int) -> IO Int
sumNeighbours grid (x,y) = do
    a <- fromMaybe 0 <$> H.lookup grid (x-1,y+1)
    b <- fromMaybe 0 <$> H.lookup grid (x  ,y+1)
    c <- fromMaybe 0 <$> H.lookup grid (x+1,y+1)
    d <- fromMaybe 0 <$> H.lookup grid (x-1,  y) 
    e <- fromMaybe 0 <$> H.lookup grid (x+1,  y) 
    f <- fromMaybe 0 <$> H.lookup grid (x-1,y-1)
    g <- fromMaybe 0 <$> H.lookup grid (x  ,y-1)
    h <- fromMaybe 0 <$> H.lookup grid (x+1,y-1)
    return $ sum [a,b,c,d,e,f,g,h]
