-- This module finds all the figure numbers using attoparsec package, and
-- return a list of figure numbers represented by @Text@ type.
{-# LANGUAGE OverloadedStrings #-}
module Text.ParseFig (
    alphaNum
  , parenFig
  , hyphenFig
  , figNo
  , (<**>)
  , figNumbers
) where

import Prelude hiding (takeWhile, concat)
import Data.Attoparsec.Text
import Data.Text (Text, cons, snoc, empty, append, concat)
import Data.Word
import Data.Char (isDigit)
import Control.Applicative (liftA2)
import Data.Maybe (catMaybes)

-- The characters that may follow '图' with parentheses considered alone.
isFigChar,isOnlyAlpha :: Char -> Bool
isFigChar= inClass "a-z0-9A-Z'‘’"
isOnlyAlpha = inClass "a-zA-Z'‘’"

alphaNum :: Parser Char
alphaNum = satisfy isFigChar

-- The sub-figure number represented by numbers or letters wrapped by parentheses
parenFig :: Parser Text
parenFig = snoc <$> (cons <$> (skipSpace *> openParen)  -- There probably exist spaces befor the open parenthesis.
                        <*> takeWhile1 isFigChar)  -- In the parentheses are letters, numbers or single quotes.
                <*> closeParen -- The matched close parenthesis.

openParen, closeParen :: Parser Char  
openParen = satisfy (\c -> c == '(' || c == '（')
closeParen = satisfy (\c -> c == ')' || c == '）')

-- The sub-figure numbers with the prefix of a hyphen such as '～', '~', '-' and '—'
hyphenFig :: Parser Text
hyphenFig = skipSpace *> hyphen  -- A hyphen representing the start of sub-figure number
              <**> skipSpace *> choice [parenFig, -- For the case of "图2-(A)"
                                        takeWhile1 isFigChar    -- For the case of "图2-2A"
                                          <**> option empty parenFig] -- or "图2-2(A)" with empty as the default value 

-- There may exist more than one hyphen
hyphen :: Parser Text
hyphen = takeWhile1 (\c -> c == '-' || c == '~' || c == '～' || c == '—' || c == '和' || c == '或')

-- Parse the full figure number
figNo :: Parser Text
figNo = cons <$> satisfy (=='图') 
             <*> (skipSpace *> takeWhile1 isDigit        -- What follows '图' must be some digits
                  <**> skipSpace *> takeWhile isOnlyAlpha  -- After the digits may follows letters or not
                  <**> option empty (manyFigs $ choice [hyphenFig, parenFig])) -- And probably there exists one 
        <?> "Figure Number"                               -- sub-figure number or more, otherwise return empty  
-- Combinator for joining parsers continually
(<**>) :: Parser Text -> Parser Text -> Parser Text
(<**>) = liftA2 append
infixl 3 <**>
-- Combinator for parse many figures and join them into a single text
manyFigs :: Parser Text -> Parser Text 
manyFigs = (concat <$>) . many1
-- Parse all the figure numbers in a text and wrap the result in @Just@, otherwise return @Nothing@
figNumbers :: Parser [Text]
figNumbers = catMaybes <$> many' (choice [Just <$> figNo, Nothing <$ anyChar])