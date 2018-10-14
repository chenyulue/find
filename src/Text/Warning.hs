-- This module finds all the user-defined warning words that is
-- not appropariate to appear in the Description of a patent.
module Text.Warning (
    warning
) where 

import Prelude hiding (lines)
import Data.Attoparsec.Text 
import Data.Maybe (catMaybes)
import Data.Text (Text, lines)
-- The input contains the user-defined words with one word per line,
-- and return a parser for warning words
warning :: [Text] -> Parser Text 
warning = choice . map string
-- Find all the warning words in a text.
allWarnings :: [Text] -> Parser [Text]
allWarnings txt = catMaybes <$> many' (choice [Just <$> warning txt, Nothing <$ anyChar])