{-# LANGUAGE OverloadedStrings #-}

-- | Extract item names from pret's data/items/names.asm files.
--
-- Items are listed as `li "NAME"` entries, 1-indexed (first li = item 1).
-- Item 0 (NO_ITEM) is implicit — it's not in the names file.

module Extract.Items (extractItems, itemsHeader) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec

import Extract.ASM


-- | Extract item names from one gen's items/names.asm.
-- Returns CSV rows: [gen, id, name]
extractItems :: Text -> FilePath -> IO [[Text]]
extractItems gen path = do
  names <- parseFile parseItemNames path
  pure [formatRow gen itemId name | (itemId, name) <- zip [1..] names]

-- | Scan for `li "NAME"` lines, skipping everything else.
parseItemNames :: Parser [Text]
parseItemNames = go []
  where
    go acc = do
      done <- option False (True <$ eof)
      if done
        then pure (reverse acc)
        else do
          horizontalSpace
          choice
            [ try (parseLi >>= \name -> go (name : acc))
            , restOfLine >> go acc
            ]

    parseLi = do
      _ <- keyword "li"
      _ <- single '"'
      name <- takeWhileP (Just "item name") (/= '"')
      _ <- single '"'
      restOfLine
      pure name

    restOfLine = takeWhileP Nothing (/= '\n') *> endOfLine

formatRow :: Text -> Int -> Text -> [Text]
formatRow gen itemId name = [gen, T.pack (show itemId), name]

itemsHeader :: [Text]
itemsHeader = ["gen", "id", "name"]
