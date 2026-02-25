module Main where

import Pokemon.Types

main :: IO ()
main = do
  putStrLn "cinnabar-coast 0.1.0"
  putStrLn $ "Gen range: " ++ show [Gen1 .. Gen2]
  putStrLn $ "Max DVs:   " ++ show maxDVs
  putStrLn $ "HP DV:     " ++ show (dvHP maxDVs)
  putStrLn $ "Shiny?     " ++ show (isShiny maxDVs)
