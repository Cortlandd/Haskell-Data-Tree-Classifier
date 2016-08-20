-- Implementing a decision tree classifier

{-
A decision tree is a model for classifying data effectively.

I will be implementing the ID3 decision tree algorithm in Haskell.
Notes: doesn't guarantee an optimal solution, may be computationaally inefficient, ID3 only supports discrete data.

The weather data("input.csv") is represented with four attributes, namely outlook, temperature, humidity, and wind. The last column represents whether it is a good idea to play outside. 

-}

-- Necessary library imports
import Data.List (nub, elemIndices)
import qualified Data.Map as M
import Data.Map (Map, (!))
import Data.List (transpose)
import Text.CSV

-- Better type synonyms for better understanding
-- of what data is being passed around.
type Class = String
type Feature = String
type Entropy = Double
type DataSet = [([String], Class)]

-- A function ton read the CSV file and handle errors
main :: IO ()
main = do
	rawCSV <- parseCSVFromFile "input.csv"
	either handleError doWork rawCSV

handleError = error "invalid file"

-- IF file is read successfully
-- THEN remove any invalid CSV records and construct a decision tree out of it
doWork :: IO ()
doWork csv = do
	let removeInvalids = filter (\x -> length x > 1)
	let myData = map (\x -> (init x, last x)) $ remoteInvalids csv
	print $ dtree "root" myData

-- Helper functions to break up the DataSEt tuple into
-- a list of samples or list of classes.
samples :: DataSet -> [[String]]
samples data = map fst data

classes :: DataSet -> [Class]
classes data = map snd data

-- Calculate the entropy of a list of values
entropy :: (Eq a) => [a] -> Entropy
entropy xs = sum $ map (\x -> prob x * into x) $ nub xs
	where prob x = (length' (elemIndices x xs))
		(length' xs)
			into x = negate $ logBase 2 (prob x)
			length' xs = fromIntegral $ length xs

-- Split an attribute by its features
splitAttr :: [(Feature, Class)] -> Map Feature [Class]
splitAttr dc = foldl (\m (f,c) -> 
	M.insertWith (++) f [c] m)
		M.empty fc

-- Obtain each of the entropies from splitting up an attribute by its features.
splitEntropy :: Map Feature [Class] -> M.Map Feature Entropy
splitEntropy m = M.map entropy m

-- Compute the information gained from splitting up
-- an attribute by its features
informationGain :: [Class] -> [(Feature, Class)] -> Double
informationGain s a = entropy s - newInformation
  where eMap = splitEntropy $ splitAttr a
  	m = splitAttr a
    toDouble x = read x :: Double
    ratio x y = (fromIntegral x) / (fromIntegral y)
    sumE = M.map (\x -> (fromIntegral.length) x / (fromIntegral.length) s) m
    newInformation = M.foldWithKey (\k a b -> b + a*(eMap!k)) 0 sumE

-- Determine which attribute contributes the highest information gain
highestInformationGain :: DataSet -> Int
highestInformationGain d = snd $ maximum $ 
  zip (map ((informationGain . classes) d) attrs) [0..]
  where attrs = map (attr d) [0..s-1]
    attr d n = map (\(xs,x) -> (xs!!n,x)) d
    s = (length . fst . head) d
        
-- Define a data structure for a decision tree that'll be constructed
data DTree = DTree { feature :: String, children :: [DTree] } 
	| Node String String deriving Show

-- Split up the dataset by the attributes that contributes the highest
-- information gain
datatrees :: DataSet -> Map String DataSet
datatrees d = 
  foldl (\m (x,n) -> M.insertWith (++) (x!!i) [((x `dropAt` i), fst (cs!!n))] m)
    M.empty (zip (samples d) [0..])
  where i = highestInformationGain d
  	dropAt xs i = let (a,b) = splitAt i xs in a ++ drop 1 b
    cs = zip (classes d) [0..]

-- A helper function to determine if all elements of a list are equal.
-- Used to check fi further splitting of a dataset is necessary by checking
-- if its classes are identical. 
allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual [x] = True
allEqual (x:xs) = x == (head xs) && allEqual xs

-- Construct the decision tree from a lebeling and a dataset of samples
dtree :: String -> DataSet -> DTree
dtree f d 
	| allEqual (classes d) = Node f $ head (classes d) 
	| otherwise = DTree f $ M.foldWithKey (\k a b -> b ++ [dtree k a] ) [] (datatrees d)







