module Main where

import qualified Data.Map.Strict as Map
import Pokemon.Types
import Pokemon.Data (loadGameData)

main :: IO ()
main = do
  putStrLn "Loading Gen 1 data..."
  g1 <- loadGameData Gen1
  putStrLn $ "  Species: " ++ show (Map.size (gameSpecies g1))
  putStrLn $ "  Moves:   " ++ show (Map.size (gameMoves g1))
  putStrLn $ "  TM/HMs:  " ++ show (Map.size (gameMachines g1))

  putStrLn "\nLoading Gen 2 data..."
  g2 <- loadGameData Gen2
  putStrLn $ "  Species: " ++ show (Map.size (gameSpecies g2))
  putStrLn $ "  Moves:   " ++ show (Map.size (gameMoves g2))
  putStrLn $ "  TM/HMs:  " ++ show (Map.size (gameMachines g2))
  putStrLn $ "  Egg moves: " ++ show (Map.size (gameEggMoves g2)) ++ " species"
  putStrLn $ "  Tutor:   " ++ show (Map.size (gameTutorMoves g2)) ++ " species"
  putStrLn $ "  Items:   " ++ show (Map.size (gameItems g2))

  -- Quick sanity check: look up Pikachu
  case Map.lookup 25 (gameSpecies g1) of
    Nothing -> putStrLn "\nERROR: Pikachu not found!"
    Just pika -> do
      putStrLn $ "\nPikachu (Gen 1): " ++ show pika
      putStrLn $ "  Types: " ++ show (speciesTypes pika)
      putStrLn $ "  Base stats: " ++ show (speciesBaseStats pika)
      putStrLn $ "  Growth rate: " ++ show (speciesGrowthRate pika)

  -- Look up a move
  case Map.lookup 85 (gameMoves g1) of
    Nothing -> putStrLn "\nERROR: Thunderbolt not found!"
    Just tb -> putStrLn $ "\nThunderbolt: " ++ show tb
