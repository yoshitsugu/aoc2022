module Main where

import Data.Foldable (foldl')
import Data.List (sort)
import Data.List.Split

main :: IO ()
main = do
    contents <- lines <$> getContents
    let elves = splitOn [""] contents
    -- print . maximum . map (foldl' (\r c -> r + read c) 0) $ elves
    print . foldl' (+) 0 . take 3 . reverse . sort . map (foldl' (\r c -> r + read c) 0) $ elves
