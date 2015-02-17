{-# LANGUAGE OverloadedStrings, Arrows #-} 
module Main where
import Text.XML.HXT.Core
import Text.XML.HXT.Arrow.XmlState.RunIOStateArrow

data MyState = MyState {
    localImages :: [FilePath]
  } deriving Show

type MyArrow = IOSLA MyState XmlTree XmlTree

main = do
  raw <- getContents
  let s = MyState []
  (xios, res) <- runIOSLA (process raw) (initialState s) undefined
  print res
  print (localImages $ xios)

process s = (readString [withValidate no, withParseHTML yes, withInputEncoding utf8] s
              >>> processChildren (processDocumentRootElement `when` isElem) 
              >>> writeDocumentToString [withIndent yes] 
              )

processDocumentRootElement
     = this         -- substitute this by the real application



