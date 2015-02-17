{-# LANGUAGE OverloadedStrings, Arrows #-} 
module Main where
import Text.XML.HXT.Core
import Text.XML.HXT.Arrow.XmlState.RunIOStateArrow
import System.Process

data MyState = MyState {
    localImages :: [FilePath]
  } deriving Show

type MyArrow = IOSLA MyState XmlTree XmlTree

main = do
  raw <- getContents
  let s = MyState []
  (_, res) <- runIOSLA (process raw) (initialState s) undefined
  print res

process s = (readString [withValidate no, withParseHTML yes, withInputEncoding utf8] s
              >>> processChildren (processDocumentRootElement `when` isElem) 
              >>> (writeDocumentToString [withIndent yes]  &&& getUserState)
              )

processDocumentRootElement
     = deep (
        hasName "img"
        >>> processAttrl ( processSrc `when` hasName "src" )
        )

processSrc :: IOSLA (XIOState MyState) XmlTree XmlTree
processSrc = 
    replaceChildren 
      (xshow getChildren
      >>> arrIO ioAction 
      >>> perform 
            -- changeState :: (s -> b -> s) -> a b b
            -- changeUserState :: (b -> s -> s) -> IOStateArrow s b b
            (changeUserState (\filePath s -> MyState $ filePath :(localImages s))) 
            
      >>> mkText)


ioAction :: String -> IO String
ioAction src = do
  (_, out, _) <- readProcessWithExitCode "./test.sh" [src] src
  return out 




