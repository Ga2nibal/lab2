module Modules.ParseCsv where 

import Modules.Helpers
import System.IO
import Data.Conduit
import Control.Monad.IO.Class
import Data.List.Split
import qualified Data.Conduit.List as CL

source :: String -> Source IO String
source filepath = do
    handle <- liftIO $ openFile filepath ReadMode
    loop handle
  where
    loop handle = do
        eof <- liftIO $ hIsEOF handle
        if eof
            then return ()
            else do
                c <- liftIO $ hGetLine handle
                yield c
                loop handle

fileSink :: Handle -> Sink String IO ()
fileSink handle = CL.mapM_ (hPutStrLn handle)

conduit :: (String -> [String]) -> ([Double] -> [Double]) -> Conduit String IO DataVector
conduit elementSplit lTransform = do
    str <- await

    case str of 
            Just s -> do
                let (res, className) = splitInClassNameAndVector . emptyFilter . elementSplit $ s
                if className /= "" then yield (lTransform res, className) else return ()
                conduit elementSplit lTransform
            _ -> return ()  


splitInClassNameAndVector :: [String] -> DataVector
splitInClassNameAndVector [] = ([], "")
splitInClassNameAndVector v = (vToDouble $ init v, last v)

vToDouble = map toDouble
toDouble = \x -> read x :: Double
emptyFilter = filter (/= "")

emptyFilter2 :: [DataVector] -> [DataVector]
emptyFilter2 = filter (\(_, name) -> name /= "")

elementSplitter :: String -> String -> [String]
elementSplitter devider = splitOneOf ("\n\r\65279\&" ++ devider)