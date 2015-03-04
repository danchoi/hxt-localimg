{-# LANGUAGE OverloadedStrings, Arrows, RecordWildCards #-} 
module Main where
import Text.XML.HXT.Core
import Text.XML.HXT.Arrow.XmlState.RunIOStateArrow
import System.Process
import System.FilePath.Posix

type ImageExtractionArrow = IOSLA (XIOState ImageExtractionState) XmlTree XmlTree

main = do
  -- get html content as String
  raw <- getContents
  -- This is an IO function that the consuming code defines.
  -- The input is the "src" attribute of every image in the HTML.
  -- The return value should be a CID identifier string and a filepath
  -- to the binary data of the image.
  -- If the src value is a data-uri, save a binary version of the content 
  -- somewhere and then return the filepath to that temporary file.

  let srcProcessFunction :: String -> IO (CID, FilePath)
      srcProcessFunction src = return ("cid:test.jpg", "test.jpg")

  (html, extractionState) <- processHtmlBody raw srcProcessFunction
  -- output the processed html
  putStrLn html
  -- output the [(CID, FilePath)] of the inlined images
  print $ inlineImages extractionState


------------------------------------------------------------------------

processHtmlBody :: String   
                -> (ImgSrcString -> IO (CID, FilePath))
                -> IO (String, ImageExtractionState)
processHtmlBody rawHtml iofunc = do
  let s = ImageExtractionState iofunc []
  (_, (x:_)) <- runIOSLA (process rawHtml) (initialState s) undefined
  return x

type CID = String
type ImgSrcString = String

{- This is the parametered function embedded in the ImageExtractionState to 
deal arbitrarily with img SRC values of any type.  This IO action can look up
binary data in a DB, look up a file, or extract a data URI value and save it in
binary to a temporary file.  -}

data ImageExtractionState = ImageExtractionState {
    processImageSrc :: String -> IO (CID, FilePath)
  , inlineImages :: [(CID,FilePath)]
  } 


{- Takes an img SRC and returns 
    - CID string to put into the src attribute
    - filepath to the image binary data
-}
ioAction :: String -> ImageExtractionState -> IO (String, FilePath)
ioAction src ImageExtractionState{..} = do
  (newSrc, filepath) <- processImageSrc src
  return (newSrc, filepath)

process :: String -> IOSLA (XIOState ImageExtractionState) a (String, ImageExtractionState)
process s = (readString [withValidate no, withParseHTML yes, withInputEncoding utf8] s
              >>> processDocumentRootElement 
              >>> (writeDocumentToString [withIndent yes]  &&& getUserState))

processDocumentRootElement :: ImageExtractionArrow
processDocumentRootElement
     = processTopDown (processImg `when` (isElem >>> hasName "img" >>> hasAttr "src"))

-- The incoming arrow at this point is an img element node
processImg :: ImageExtractionArrow
processImg = processAttrl (processSrc `when` hasName "src") 

processSrc :: IOSLA (XIOState ImageExtractionState) XmlTree XmlTree
processSrc = 
    replaceChildren (
      (xshow getChildren &&& getUserState)
      >>> 
      arrIO2 ioAction 
      >>> 
      changeUserState 
         (\(newSrc, filepath) s -> s { inlineImages = (newSrc, filepath):(inlineImages s)})
      >>>
      arr fst 
      >>> 
      mkText

    )


