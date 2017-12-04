import qualified Data.HashTable.IO as H
import Data.Maybe (fromMaybe)

main :: IO ()
main = do

    print $ partOne input

    print =<< partTwo input
    
input :: Int
input = 361527

partOne :: Int -> Int
partOne n = (\(px, py) -> abs px + abs py)
          $ path n

type Grid = H.BasicHashTable (Int, Int) Int

directions = cycle [ ( 1, 0)
                   , ( 0, 1)
                   , (-1, 0)
                   , ( 0,-1) ]

lengths = concatMap (\x -> [x, x]) [1..]

steps = concat . zipWith replicate lengths $ directions

path n = foldr (\(px,py) (dx, dy) -> (px + dx, py + dy))
               (0,0)
               (take (n-1) steps)

partTwo n = do
    grid <- H.new :: IO Grid
    H.insert grid (0, 0) 1
    go grid (0,0) steps
    where
    go grid (px,py) ((dx,dy):steps) = do

        let px' = px + dx
            py' = py + dy

        s <- sumNeighbours grid (px',py')

        if s > n
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
