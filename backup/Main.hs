{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.ParseFig
import Data.Attoparsec.Text
import qualified Data.Text.IO as T
import Data.Text (Text)
import Text.DealWith
import Text.Warning
import Data.Maybe (catMaybes)

main :: IO ()
main = do
    descp  <- T.readFile "C:\\Users\\chenyulue\\Documents\\Haskell\\find-v1\\test.txt"
    figure <- T.readFile "C:\\Users\\chenyulue\\Documents\\Haskell\\find-v1\\test-fig.txt"
    warn <- T.readFile "C:\\Users\\chenyulue\\Documents\\Haskell\\find-v1\\warning.txt"
    let desp = getResult (figAndWarn warn) descp
        figs = getResult figNumbers figure
    mapM_ T.putStrLn $ dealWith desp figs

getResult :: Parser [a] -> Text -> [a]
getResult p input = 
    either (const []) id $ 
        parseOnly p input

-- Find all the fingure numbers and warning words
figAndWarn :: Text -> Parser [Either Text Text]
figAndWarn txt = 
    catMaybes <$> 
      many' (choice [Just . Left <$> figNo,  -- Figure numbers wrapped by `Left`
                     Just . Right <$> warning txt, -- Warning words wrapped by `Right`
                     Nothing <$ anyChar]) -- Other words return `Nothing`
