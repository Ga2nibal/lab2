import Modules.Helpers
import Modules.Bayes
import Modules.ParseCsv
import Modules.Arguments

import System.IO
import Data.Conduit
import System.Random
import Control.Monad.State
import Options.Applicative
import Data.List
import Text.Printf
import qualified Data.Conduit.List as CL
import qualified Data.Map as M

naiveBayes :: Options -> IO ()
naiveBayes (Options filePath output retryCount studyPercent devider skipFirstLine skipFirstCol skipLastCol) = do
    let normStudyPercent = max (min studyPercent 100) 10

    let result :: IO [DataVector]
        source' = source filePath
        lTransform = listTransform skipFirstCol skipLastCol
        conduit' = conduit (elementSplitter devider) lTransform
        result = source' $$ conduit' =$ CL.consume
    v <- result
    let vectors = if skipFirstLine 
                  then tail v 
                  else v
    gen <- getStdGen

    let res = bestTrainedData vectors retryCount normStudyPercent gen    
    let resultSource = CL.sourceList $ bayesResultSource res

    case output of
        Just outFile -> do 
            h <- openFile outFile WriteMode
            resultSource $$ (fileSink h)
            hClose h
        Nothing -> resultSource $$ awaitForever (lift . print)
    


printAttribute :: (Int, (Double, Double)) -> String
printAttribute (index, (mu, disp)) = printf " - %d(%.2f;%.2f)" index mu disp

printTrainedClass :: (String , M.Map Int (Double, Double)) -> String
printTrainedClass (className, attrMap) = className ++ arrtName
                                            where attrNames = map printAttribute $ sortOn fst (M.toList attrMap)
                                                  arrtName = concat attrNames

bayesResultSource :: TrainedData -> [String]
bayesResultSource trained = map printTrainedClass trainedList
                                where trainedClassF attrData = M.map matAndDisp attrData
                                      trainedList = M.toList . M.map trainedClassF $ trained


consoleSink :: Sink String IO ()
consoleSink = awaitForever $ liftIO . putStrLn

main :: IO ()
main = execParser opts >>= naiveBayes 

