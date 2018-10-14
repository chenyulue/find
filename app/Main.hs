module Main where

import           Data.Attoparsec.Text
import           Data.Maybe           (catMaybes)
import           Data.Text            (Text, empty)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           GUI
import           Text.DealWith
import           Text.ParseFig
import           Text.Warning

main :: IO ()
main =
    gui $ \text0 text1 text2 -> (let warnWd = filter (/= empty) (T.lines text0)
                                     desp = getResult (figAndWarn warnWd)
                                     figs = getResult figNumbers
                                  in dealWith (desp text1) (figs text2))

getResult :: Parser [a] -> Text -> [a]
getResult p input =
    either (const []) id $
        parseOnly p input

-- Find all the fingure numbers and warning words
figAndWarn :: [Text] -> Parser [Either Text Text]
figAndWarn txt =
    catMaybes <$>
      many' (choice [Just . Left <$> figNo,  -- Figure numbers wrapped by `Left`
                     Just . Right <$> warning txt, -- Warning words wrapped by `Right`
                     Nothing <$ anyChar]) -- Other words return `Nothing`
