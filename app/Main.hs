module Main where

import qualified Data.Map.Strict as Map
import Pokemon.Types
import Pokemon.Data (loadGameData)

main :: IO ()
main = do
  putStrLn "Loading Gen 1 data..."
  g1 <- loadGameData Gen1
  putStrLn $ "  Species: " ++ show (Map.size (gdSpecies g1))
  putStrLn $ "  Moves:   " ++ show (Map.size (gdMoves g1))
  putStrLn $ "  TM/HMs:  " ++ show (Map.size (gdMachines g1))

  putStrLn "\nLoading Gen 2 data..."
  g2 <- loadGameData Gen2
  putStrLn $ "  Species: " ++ show (Map.size (gdSpecies g2))
  putStrLn $ "  Moves:   " ++ show (Map.size (gdMoves g2))
  putStrLn $ "  TM/HMs:  " ++ show (Map.size (gdMachines g2))
  putStrLn $ "  Egg moves: " ++ show (Map.size (gdEggMoves g2)) ++ " species"
  putStrLn $ "  Tutor:   " ++ show (Map.size (gdTutorMoves g2)) ++ " species"
  putStrLn $ "  Items:   " ++ show (Map.size (gdItems g2))

  -- Quick sanity check: look up Pikachu
  case Map.lookup 25 (gdSpecies g1) of
    Nothing -> putStrLn "\nERROR: Pikachu not found!"
    Just pika -> do
      putStrLn $ "\nPikachu (Gen 1): " ++ show pika
      putStrLn $ "  Types: " ++ show (specTypes pika)
      putStrLn $ "  Base stats: " ++ show (specBaseStats pika)
      putStrLn $ "  Growth rate: " ++ show (specGrowthRate pika)

  -- Look up a move
  case Map.lookup 85 (gdMoves g1) of
    Nothing -> putStrLn "\nERROR: Thunderbolt not found!"
    Just tb -> putStrLn $ "\nThunderbolt: " ++ show tb
