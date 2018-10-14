-- This module contains some useful functions that deal with the parse result
{-# LANGUAGE OverloadedStrings #-}
module Text.DealWith (
    dealWith
  , (<??>)
) where

import Prelude hiding (intercalate, unlines)
import Data.Text (Text, intercalate, unlines, append, split, empty)
import Data.List (nub, sort, (\\))
import Data.Either

-- Deal with the result and change it into a human-readable text        
dealWith :: [Either Text Text] -> [Text] -> [[Text]]
dealWith xs figs = 
    -- Change `Left` constructors into `Right` constructors in the first group
    -- while keep the `Right` constructors in the second group
    let result = (lefts xs, rights xs)
     in output result figs

type DespTxt = Text  -- Represent the parse result for the description
type FigsTxt = Text  -- Represent the parse result for the figures
-- Compare the figure numbers between the descriptioin and the figures
infix 1 <??>
(<??>) :: [DespTxt] -> [FigsTxt] -> [Text] 
desp <??> figs = 
    let (x, y) = (delSort desp, delSort figs)
     in case (x \\ y, y \\ x) of
          ([],[]) -> "【说明书中出现的图号】:" : intercalate "，" x : 
                        "【附图中出现的图号】：" : intercalate "，" y :
                        "【比较结果】：" : "说明书与附图中的图号一一对应" : [empty, empty, empty]
          ([],a)  -> "【说明书中出现的图号】:" : intercalate "，" x :
                        "【附图中出现的图号】：" : intercalate "，" y :
                        "【比较结果】：" : empty : empty : "附图中多出的额外图号包括：" : [intercalate "，" a]
          (a,[])  -> "【说明书中出现的图号】:" : intercalate "，" x :
                        "【附图中出现的图号】：" : intercalate "，" y :
                        "【比较结果】：" : "说明书中多出的额外图号包括：" : intercalate "，" a : [empty, empty]
          (a,b)   -> "【说明书中出现的图号】:" : intercalate "，" x :
                        "【附图中出现的图号】：" : intercalate "，" y :
                        "【比较结果】：" : "说明书中多出的额外图号包括：" : intercalate "，" a 
                        : "附图中额外多出的图号包括：" : [intercalate "，" b]

-- Output the result and turn it into Text
output :: ([Text],[Text]) -> [Text] -> [[Text]]
output (figs,[]) ss = 
    [figs <??> ss, ["未找到相关敏感词"]]
output (figs,warns) ss | not (null warns) = 
    [figs <??> ss, delSort warns]

-- Delete and sort the duplicate words in the result
delSort :: [Text] -> [Text]
delSort = sort . nub . concatMap splitAndOr

splitAndOr :: Text -> [Text]
splitAndOr txt 
    | (a:b:_) <- split (\c -> c == '和' || c == '或') txt = a : ("图" `append` b) : [] 
    | otherwise = [txt]