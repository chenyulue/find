-- This module deals with the database of sensitive words, 
-- including adding, searching, and deleting a specific word.
{-# LANGUAGE OverloadedStrings #-}
module Data.SensitiveWords where

import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Text (Text, append, pack, cons)
import Data.List ((\\), intersect, delete)
import System.IO.Error

addFromFile :: FilePath   -- The database filepath
            -> FilePath   -- The input filepath
            -> IO Text
addFromFile original newWords = do
    inputText <- T.readFile newWords `catchIOError` \_ -> return T.empty
    contents <- T.readFile original `catchIOError` \_ -> return T.empty
    let origText = filter (/= T.empty) (T.lines contents )
        newText = filter (/= T.empty) (T.lines inputText)
        diffText = newText \\ origText
        commonText = intersect newText origText
    case (length diffText, length commonText) of 
        (0,0) -> return $ "【！待添加文件为空！】"
        (_,0) -> do T.appendFile original ("\n" `append` T.intercalate "\n" diffText)
                    return $ "【所有敏感词均已添加】\n\n【原始敏感词如下】：\n" `append` 
                        T.intercalate "，" origText `append` "\n\n【新添加的敏感词如下】：\n"
                        `append` T.intercalate "，" newText
        (0,_) -> return $ "【待添加的敏感词已在词库中】\n\n【词库中的敏感词如下】：\n" `append`
                        T.intercalate "，" origText
        (n,m) -> do T.appendFile original ("\n" `append` T.intercalate "\n" diffText)
                    return $ "【" `append` pack (show m) `append` "个重复词，" `append` pack (show n) `append` 
                         "个新词被添加】\n\n【原始敏感词如下】：\n" `append` T.intercalate "，" origText
                         `append` "\n\n【新添加的敏感词如下】：\n" `append` T.intercalate "，" diffText

addOneWord :: FilePath -> Text -> IO Text
addOneWord original word = do 
    contents <- T.readFile original 
    let origText = filter (/= T.empty) (T.lines contents)
    case word of 
        "" -> return "【！请输入一个词再点击添加】"
        _  -> if word `elem` origText then 
                   return $ "【“" `append` word `append` "”已在敏感词库中】"
              else do T.appendFile original ('\n' `cons` word)
                      return $ "【“" `append` word `append` "”已添加】"

searchWord :: FilePath -> Text -> IO Text
searchWord original word = do 
    contents <- T.readFile original 
    let origText = filter (/= T.empty) (T.lines contents)
    case word of 
        "" -> return $ "【所有敏感词列表】：\n" `append` T.intercalate "，" origText 
        _  -> if word `elem` origText then 
                   return $ "【查找到敏感词】：" `append` word
              else return $ "【！该词不在敏感词库中】" 

deleteWord :: FilePath -> Text -> IO Text
deleteWord original word = do 
    contents <- T.readFile original 
    let origText = filter (/= T.empty) (T.lines contents)
    case word of 
        "DELETEALL" -> T.writeFile original "" >> return "【！所有敏感词已被删除】"
        "" -> return "【！请输入一个词再点击删除】"
        _ -> if word `elem` origText then 
                  do T.writeFile original (T.intercalate "\n" . delete word $ origText)
                     return $ "【“" `append` word `append` "”已删除】"
             else return "【！该词不在敏感词库中】"