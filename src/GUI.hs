
-- This module designs the GUI for this application
{-# LANGUAGE OverloadedStrings #-}
module GUI where

import Graphics.UI.Gtk
import Text.ParseFig
import Data.Attoparsec.Text
import qualified Data.Text.IO as T
import Data.Text (Text)
import qualified Data.Text as T
import Text.DealWith
import Text.Warning
import Data.Maybe (catMaybes)
import Data.SensitiveWords
import Data.Text.Encoding

database :: FilePath 
database = "warning.dat"
helpDocPath :: FilePath 
helpDocPath = "help.dat"

gui :: (Text -> Text -> Text -> [[Text]]) -> IO ()
gui fn = do 
    _ <- initGUI 
    xml <- builderNew
    builderAddFromFile xml ("findGUI.glade" :: String)
    window <- builderGetObject xml castToWindow ("mainWindow" :: String)
    helpButton <- builderGetObject xml castToButton ("help" :: String)
    addButton <- builderGetObject xml castToButton ("add" :: String)
    cmpButton <- builderGetObject xml castToButton ("compare" :: String)
    despView <- builderGetObject xml castToTextView ("descpTextView" :: String)
    figsView <- builderGetObject xml castToTextView ("figsTextView" :: String)
    comprView <- builderGetObject xml castToTextView ("comprTextView" :: String)
    warningView <- builderGetObject xml castToTextView ("warningTextView" :: String)
    onClicked cmpButton $ do warningWords <- T.readFile database
                             compareAndFind (fn warningWords) despView figsView comprView warningView
    onClicked helpButton $ helpDialog helpDocPath
    onClicked addButton addDialog
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI

compareAndFind :: TextViewClass cls => (Text -> Text -> [[Text]]) 
                                    -> cls -> cls -> cls -> cls -> IO ()
compareAndFind fn cls1 cls2 cls3 cls4 = do
    text1       <- textViewGetValue cls1
    text2       <- textViewGetValue cls2
    textbuffer3 <- textViewGetBuffer cls3
    textbuffer4 <- textViewGetBuffer cls4
    let (cmpr:warn:_) = fn text1 text2
        (despR, figsR) = (T.splitOn "，" (cmpr !! 6), T.splitOn "，" (cmpr !! 8))
    txtTagTable1 <- textTagTableNew
    txtTagTable2 <- textTagTableNew
    buff1 <- textBufferNew (Just txtTagTable1)
    buff2 <- textBufferNew (Just txtTagTable2)
    textBufferSetText buff1 (T.unpack text1)
    textBufferSetText buff2 (T.unpack text2)
    textBufferSetByteString textbuffer3 $ encodeUtf8 (T.unlines cmpr)
    textBufferSetByteString textbuffer4 $ encodeUtf8 (T.unlines warn)
    textViewSetBuffer cls3 textbuffer3
    textViewSetBuffer cls4 textbuffer4
    start1      <- textBufferGetStartIter buff1
    start2      <- textBufferGetStartIter buff2
    tagRed <- textTagNew (Just ("red" :: Text))
    tagYellow <- textTagNew (Just ("yellow" :: Text))
    tagYellow2 <- textTagNew (Just ("yellow2" :: Text))
    set tagRed [textTagBackground := ("Red" :: String)]
    set tagYellow [textTagBackground := ("Yellow" :: String)]
    set tagYellow2 [textTagBackground := ("Yellow" :: String)]
    textTagTableGetSize txtTagTable1
    textTagTableGetSize txtTagTable2
    textTagTableAdd txtTagTable1 tagRed 
    textTagTableAdd txtTagTable1 tagYellow
    textTagTableAdd txtTagTable2 tagYellow2
    mapM_ (highlight buff1 start1 txtTagTable1 ("yellow" :: Text)) despR
    mapM_ (highlight buff1 start1 txtTagTable1 ("red" :: Text)) warn
    mapM_ (highlight buff2 start2 txtTagTable2 ("yellow2" :: Text)) figsR 
    textViewSetBuffer cls1 buff1
    textViewSetBuffer cls2 buff2



highlight :: TextBuffer -> TextIter -> TextTagTable -> TagName -> Text -> IO ()
highlight buffer start table tagName str = do 
    tag <- textTagTableLookup table tagName
    case (tag, str) of 
        (Nothing, _) -> return ()
        (_, "") -> return ()
        (Just tag', _) -> do 
            pos <- textIterForwardSearch start str [] Nothing 
            case pos of 
                Nothing -> return ()
                Just (s,e) -> do textBufferApplyTag buffer tag' s e
                                 highlight buffer e table tagName str

textViewGetValue :: TextViewClass self => self -> IO Text
textViewGetValue tv = do
    buf   <- textViewGetBuffer tv
    start <- textBufferGetStartIter buf
    end   <- textBufferGetEndIter buf
    decodeUtf8 <$> textBufferGetByteString buf start end False

helpDialog :: FilePath -> IO ()
helpDialog path = do    
    xml <- builderNew 
    builderAddFromFile xml ("findGUI.glade" :: String)
    helpWin <- builderGetObject xml castToDialog ("helpDialog" :: String)
    closeButton <- builderGetObject xml castToButton ("closeButton" :: String)
    aboutButton <- builderGetObject xml castToButton ("aboutButton" :: String)
    onClicked closeButton $ widgetDestroy helpWin
    onClicked aboutButton $ aboutDialog
    helpDoc <- builderGetObject xml castToTextView ("helpDoc" :: String)
    helpBuffer <- textViewGetBuffer helpDoc
    doc <- T.readFile path
    textBufferSetByteString helpBuffer $ encodeUtf8 doc
    onDestroy helpWin $ widgetDestroy helpWin
    widgetShowAll helpWin

aboutDialog :: IO ()
aboutDialog = do 
    xml <- builderNew 
    builderAddFromFile xml ("findGUI.glade" :: String)
    aboutWin <- builderGetObject xml castToAboutDialog ("aboutDialog" :: String)
    widgetShowAll aboutWin

addDialog :: IO ()
addDialog = do
  xml <- builderNew
  builderAddFromFile xml ("findGUI.glade" :: String)
  addWin <- builderGetObject xml castToDialog ("addDialog" :: String)
  importBttn <- builderGetObject xml castToButton ("importButton" :: String)
  filepathEntry <- builderGetObject xml castToEntry ("filePathEntry" :: String)
  searchEntry <- builderGetObject xml castToEntry ("searchEntry" :: String)
  addBttn <- builderGetObject xml castToButton ("addButton" :: String)
  deleteBttn <- builderGetObject xml castToButton ("deleteButton" :: String)
  searchBttn <- builderGetObject xml castToButton ("searchButton" :: String)
  resultReview <- builderGetObject xml castToTextView ("listTextView" :: String)
  reviewBuffer <- textViewGetBuffer resultReview
  onClicked importBttn $ do
    selectFile filepathEntry
    fp <- entryGetText filepathEntry
    msg <- addFromFile database fp
    textBufferSetByteString reviewBuffer $ encodeUtf8 msg
  onClicked addBttn $ showResult searchEntry reviewBuffer addOneWord
  onClicked searchBttn $ showResult searchEntry reviewBuffer searchWord
  onClicked deleteBttn $ showResult searchEntry reviewBuffer deleteWord
  widgetShowAll addWin

showResult :: Entry -> TextBuffer -> (FilePath -> Text -> IO Text) -> IO ()
showResult entry buffer fn = do 
    content <- entryGetText entry 
    msg <- fn database content 
    textBufferSetByteString buffer (encodeUtf8 msg)


selectFile :: Entry -> IO ()
selectFile entry = do
  dialog <- fileChooserDialogNew
              (Just $ ("选择敏感词文件" :: String))
              Nothing                     
              FileChooserActionOpen            
              [(("选择" :: String)
               , ResponseAccept)
              ,(("取消" :: String)
               ,ResponseCancel)]
  widgetShow dialog
  response <- dialogRun dialog
  case response of
    ResponseAccept -> do Just fileName <- fileChooserGetFilename dialog
                         entrySetText entry fileName
    ResponseCancel -> entrySetText entry ("" :: String)
    ResponseDeleteEvent -> entrySetText entry ("" :: String)
  widgetHide dialog



    
