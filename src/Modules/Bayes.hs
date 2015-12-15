module Modules.Bayes(matAndDisp, splitOnTrainAndEducation, testAccuracy, randomSt, trainedCount, attrCount, bestTrainedData) where

import Modules.Helpers
import System.Random
import Control.Monad.State
import Data.List
import qualified Data.Map as M


-----------------------------------UTILS-----------------------------------

attrCount :: ClassDataMap -> Int
attrCount = M.fold (\attrs count -> count + (length attrs)) 0

trainedCount :: TrainedData -> Int
trainedCount = M.fold (\classData count -> count + (attrCount classData)) 0

randomSt :: (Num a, RandomGen g, Random a) => State g a  
randomSt = state (randomR (0, 100))

testAccuracy :: [Bool] -> Double
testAccuracy testResults = correct / total
                            where total = fromIntegral . length $ testResults
                                  correct = fromIntegral . length . filter id $ testResults

splitOnTrainAndEducation :: Int -> [DataVector] -> State StdGen ([DataVector], [DataVector])
splitOnTrainAndEducation threshold [] = return ([], [])
splitOnTrainAndEducation threshold input = do 
    let (x:xs) = input
    val <- randomSt
    (study, test) <- splitOnTrainAndEducation threshold xs
    if val <= threshold 
    then return (x:study, test)
    else return (study, x:test)

matAndDisp :: [Double] -> (Double, Double) 
matAndDisp attributes  = (m, disp)
                                    where m = (sum attributes) / (fromIntegral $ length attributes)
                                          dispNumerator = sum  $ map (\x -> (x - m) ** 2)  attributes
                                          dispDevider = fromIntegral $ (length attributes - 1)
                                          disp = dispNumerator / dispDevider

-----------------------------------TRAINING-----------------------------------

trainVectorWithIndex :: Int -> DataVector -> TrainedData -> TrainedData
trainVectorWithIndex _ ([], _) trained = trained
trainVectorWithIndex index ([value], dataClass) trained = M.alter classDataAdjust dataClass trained
                                            where classDataAdjust classData = case classData of 
                                                                                    Just cd -> Just(M.alter savedAttrAdjust index cd)
                                                                                    Nothing -> Just(M.singleton index [value])
                                                  savedAttrAdjust attributes = case attributes of
                                                                                    Just attrs -> Just(value:attrs)
                                                                                    Nothing -> Just([value])

trainVectorWithIndex index ((x:xs),dataClass) trained = trainVectorWithIndex (index + 1) (xs, dataClass) insertedValue
                                                        where insertedValue = trainVectorWithIndex index ([x], dataClass) trained
trainVector = trainVectorWithIndex 0


train' :: TrainedData -> [DataVector] -> TrainedData
train' trained []     = trained
train' trained (vector:xs) = train' (trainVector vector trained)  xs

train :: [DataVector] -> TrainedData
train = train' M.empty

-----------------------------------BAYES-----------------------------------
pXiC :: ClassDataMap -> Double -> Int -> Double
pXiC classData xi index = mult * (1.0 / devider)
                                where (m, sqDisp) = matAndDisp (classData M.! index)
                                      power = ((xi - m) ^ 2) / (-2*sqDisp)
                                      devider = sqrt (2.0 * pi * sqDisp) 
                                      mult = exp power


pC :: TrainedData -> String -> Double 
pC trained className = classCount / totalCount
                        where classCount = fromIntegral $ attrCount $ trained M.! className
                              totalCount = fromIntegral $ trainedCount trained



testBayes :: TrainedData -> [Double] -> String
testBayes input vectData = assignedClass
                                        where delta = 0.0000001
                                              pc vectClass = pC input vectClass
                                              trainedClassF className attrData = (pc className) * (product $ mapWithIndex (vectorMap attrData) vectData)
                                              vectorMap attrData (index, val) = (pXiC attrData val index) + delta
                                              classProbList = M.toList $ M.mapWithKey trainedClassF input
                                              (assignedClass, _) = last $ sortOn (\(className, prob) -> prob) classProbList


testOnData :: [DataVector] -> Int -> Int -> StdGen -> [(TrainedData, Double)]
testOnData _ 0 _ _ = []
testOnData input tryCount trainPart gen = (trained, accuracy):(testOnData input (tryCount-1) trainPart gen')
                                            where ((trainData,testData), gen') = runState (splitOnTrainAndEducation trainPart input) gen
                                                  trained = train trainData
                                                  vectMapF (vectorData, vectorClass) = (testBayes trained vectorData) == vectorClass
                                                  accuracy = testAccuracy $ map vectMapF testData

bestTrainedData :: [DataVector] -> Int -> Int -> StdGen -> TrainedData
bestTrainedData input tryCount trainPart gen = trained
                                                    where testRes = testOnData input tryCount trainPart gen
                                                          (trained, _) = last $ sortOn (\(_, acc) -> acc) testRes
