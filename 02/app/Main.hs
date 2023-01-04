module Main where

import Data.Char
import Data.Foldable (foldl')

toHand :: String -> (Int, Int)
toHand s =
    let l : _ : r : _ = s
     in (ord l - ord 'A', ord r - ord 'X')

handPoint :: (Int, Int) -> Int
handPoint (_, y) = y + 1

gamePoint :: (Int, Int) -> Int
gamePoint (0, 2) = 0
gamePoint (2, 0) = 6
gamePoint (x, y)
    | x > y = 0
    | x == y = 3
    | otherwise = 6

calcPoint :: String -> Int
calcPoint c =
    let hand = toHand c
     in gamePoint hand + handPoint hand

toHand2 :: String -> (Int, Int)
toHand2 s =
    let l : _ : r : _ = s
        lo = ord l - ord 'A'
     in case r of
            'X' -> (lo, if lo == 0 then 2 else lo - 1)
            'Y' -> (lo, lo)
            'Z' -> (lo, if lo == 2 then 0 else lo + 1)

calcPoint2 :: String -> Int
calcPoint2 c =
    let hand = toHand2 c
     in gamePoint hand + handPoint hand

main :: IO ()
main = do
    contents <- lines <$> getContents
    print $ foldl' (\r c -> r + calcPoint2 c) 0 contents
