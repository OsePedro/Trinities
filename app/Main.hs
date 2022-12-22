{-# LANGUAGE TupleSections #-}

module Main(main) where

import Control.Monad.State.Strict
import Data.Array
import Data.Char
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Ord
import qualified Data.Set as S
import System.Random
import Text.Printf


newtype Phases = Phases {phases :: [Phase]} deriving(Show)

data Phase = Phase {triplet :: Triplet, count :: Int} deriving(Show)

type Triplet = (Int,Int,Int)

data LowerBound = LowerBound {problem :: Problem, atLeast :: Int} deriving(Show)

newtype Problem = Problem {setSpecs :: S.Set SetSpec} deriving(Eq,Ord,Show)

data SetSpec =
  SetSpec {cardinality :: Int, setIndex :: Int} deriving(Eq,Ord,Show)


countTriplets :: Phases -> Int
countTriplets = sum . map count . phases

betterPhases :: Problem -> Phases
betterPhases = compress . Phases . unfoldr removeTripletFromBiggestSets

-- Pre: all cardinalities are > 0.
removeTripletFromBiggestSets :: Problem -> Maybe (Phase,Problem)
removeTripletFromBiggestSets (Problem sSpecs0)
  | S.size sSpecs0 < 3 = Nothing
  | otherwise = Just (phase, Problem sSpecs1)
  where
  (z:y:x:rest) = S.toDescList sSpecs0
  phase = Phase {triplet = mkTriplet x y z, count = 1}
  sSpecs1 =
    S.fromList (catMaybes [dec z, dec y, dec x]) `S.union` S.fromDescList rest

  dec ss  | cardinality ss == 1 = Nothing
          | otherwise = Just ss {cardinality = cardinality ss - 1}

compress :: Phases -> Phases
compress ps@(Phases []) = ps
compress (Phases (p0:ps)) = Phases $ go p0 ps
  where
  go p [] = [p]
  go prev (p:ps)
    | triplet prev == triplet p = go (prev {count = count prev + count p}) ps
    | otherwise = prev : go p ps

naivePhases :: Problem -> Phases
naivePhases =
  Phases . filter ((/=0) . count) . phaseList . S.toDescList . setSpecs
  where
  phaseList (z:y:x:rest) =
    Phase {triplet = mkTriplet x y z, count = cardinality x} :
    phaseList (sortOn Down $ z `minus` x : y `minus` x : rest)
  phaseList _ = []

  b `minus` a = b {cardinality = cardinality b - cardinality a}

mkTriplet :: SetSpec -> SetSpec -> SetSpec -> Triplet
mkTriplet x y z =
  let [i0,i1,i2] = sort [setIndex x, setIndex y, setIndex z] in (i0,i1,i2)

mkProblem :: [Int] -> Problem
mkProblem cardinalities =
  Problem $ S.fromList $ zipWith (curry setSpec) cardinalities [0..]

main = do
  testPhases "betterPhases" betterPhases
  csv <- readFile "data/TrinitySnapshotTest-12_20-12_22.csv"
  let eval = evaluate $ fromJust $ parseLowerBounds 10 csv
  eval "naivePhases" naivePhases
  eval "betterPhases" betterPhases

evaluate :: [(String,LowerBound)] -> String -> (Problem -> Phases) -> IO ()
evaluate wLBs name phases = do
  let (wallets,lbs) = unzip wLBs
  let ps = phases . problem <$> lbs
  let cmps = comparisons $ zip wallets $ zip ps lbs
  let (failures,improvements) = selectMismatches cmps

  printCSV ' ' ("Improvements by " ++ name) improvements
  printCSV ' ' ("Failures by " ++ name) failures

printCSV :: Char -> String -> [(String,(Phases,LowerBound))] -> IO ()
printCSV fill heading walletPhasesLBs
  | null walletPhasesLBs = return ()
  | otherwise = do
      printHeading fill heading
      putStrLn $ toCSV walletPhasesLBs

selectMismatches  :: [(Ordering,(String,(Phases,LowerBound)))]
                  -> ( [(String,(Phases,LowerBound))]
                     , [(String,(Phases,LowerBound))])
selectMismatches cmps = (selectComparisons cmps LT, selectComparisons cmps GT)

selectComparisons :: [(Ordering,(String,(Phases,LowerBound)))]
                  -> Ordering
                  -> [(String,(Phases,LowerBound))]
selectComparisons cmps ord = snd <$> filter ((ord ==) . fst) cmps

comparisons :: [(String,(Phases,LowerBound))]
            -> [(Ordering,(String,(Phases,LowerBound)))]
comparisons = map $ \wplb -> (uncurry compareToLowerBound (snd wplb), wplb)

printHeading :: Char -> String -> IO ()
printHeading filler heading0 =
      putStrLn ""
  >>  putStrLn line
  >>  putStr pre >> putStr heading >> putStrLn post
  >>  putStrLn line
  where
  lineLength = 80
  heading = ' ':heading0 ++ " "
  hLen = length heading
  preLen = quot (lineLength - hLen) 2
  pre = replicate preLen filler
  post = replicate (lineLength - preLen - hLen) filler
  line = replicate lineLength '='

compareToLowerBound :: Phases -> LowerBound -> Ordering
compareToLowerBound ps lb = compare (countTriplets ps) (atLeast lb)

parseLowerBounds :: Int -> String -> Maybe [(String,LowerBound)]
parseLowerBounds expectedLen =
    sequence
  . map (\(wallet,mIs) -> (wallet,) <$> toLowerBound expectedLen mIs)
  . parseCSV

toLowerBound :: Int -> [Maybe Int] -> Maybe LowerBound
toLowerBound expectedLen ns
  | length ns /= expectedLen || isNothing mLB = Nothing
  | otherwise =
    LowerBound (Problem $ S.fromList $ setSpec <$> catMaybes mSetSpecs)
      <$> (fst <$> mLB)
  where
  (mLB:mSetSpecs) = reverse $ zipWith (\mN i -> (,i) <$> mN) ns [0..]

setSpec :: (Int,Int) -> SetSpec
setSpec (card,i) = SetSpec {cardinality=card, setIndex=i}

parseCSV :: String -> [(String,[Maybe Int])]
parseCSV = filter (not . null) . map parseRow . tail . lines

parseRow :: String -> (String,[Maybe Int])
parseRow s =
  (wallet, parseInt . takeWhile (not . isSpace) . dropWhile isSpace <$> rest)
  where
  (wallet:rest) = commaSplits s
  parseInt "" = Nothing
  parseInt s = Just $ read s

commaSplits :: String -> [String]
commaSplits = unfoldr next
  where
  next "" = Nothing
  next line = let (el,rest) = span (/= ',') line
              in  Just (el, if null rest then [] else tail rest)

toCSV :: [(String,(Phases,LowerBound))] -> String
toCSV walletPhasesLBs = intercalate "\n" (header : map row walletPhasesLBs)
  where
  header =
    intercalate ","
    $   "Wallet" : map (printf "Set %d") setIs
    ++  ["Old Trinities","New Trinities"]
    ++  map (printf "Phase %d") phaseIs

  row (wallet, (ps@(Phases pList), lb)) =
    intercalate ","
    $   wallet : [M.findWithDefault "" i iToCardText | i <- setIs]
    ++  map show [atLeast lb, countTriplets ps]
    ++  map phaseStr pList
    ++  replicate (maxNoPhases - length pList) ""
    where
    iToCardText =
      M.fromList
      $   (\sSpec -> (setIndex sSpec, show $ cardinality sSpec))
      <$> (S.toList $ setSpecs $ problem lb)

    phaseStr p = "\"" <> show (count p) <> " × " <> show (triplet p) <> "\""

  setIs = [0..maxNoSets-1]
  phaseIs = [0..maxNoPhases-1]
  maxNoPhases = maximum $ 0 : map noPhases walletPhasesLBs
  maxNoSets = maximum $ 0 : map noSets walletPhasesLBs
  noPhases = length . phases . fst . snd
  noSets = S.size . setSpecs . problem . snd . snd

testPhases :: String -> (Problem -> Phases) -> IO ()
testPhases name phases = do
  printCSV '#' ("Test Failures by " ++ name) failures
  printCSV '#' ("Test Improvements by " ++ name) improvements
  where
  maxNoSets = 8
  maxNoTriplets = 100
  sLBs = sequence $
    [ randomLowerBound noSets noTriplets
    | noSets <- [4..maxNoSets]
    , let minNoTriplets = ceiling (fromIntegral noSets / 3) * 2
    , noTriplets <- [minNoTriplets .. maxNoTriplets]
    ]
  lbs = evalState sLBs $ mkStdGen 0
  ps = phases . problem <$> lbs
  walletPhasesLBs = zip (show <$> [0..]) $ zip ps lbs
  (failures,improvements) = selectMismatches $ comparisons walletPhasesLBs

-- Pre: noSets >= 3.
randomLowerBound :: Int -> Int -> State StdGen LowerBound
randomLowerBound noSets noTriplets = do
  triplets <- sequence $ replicate noTriplets $ randomTriplet noSets
  let cardinalities = accumArray (+) 0 (0,noSets-1) $ concatMap incs triplets
  let prob = mkProblem $ elems cardinalities

  if allNonZero triplets
  then return $ LowerBound prob noTriplets
  else randomLowerBound noSets noTriplets

  where
  tripList (i,j,k) = [i,j,k]
  incs = (`zip` repeat 1) . tripList
  allNonZero triplets =
    noSets == S.size (S.fromList $ concatMap tripList triplets)

-- Pre: noSets >= 3.
randomTriplet :: Int -> State StdGen Triplet
randomTriplet noSets = do
  (i,is1) <- removeRndIndex [0..noSets-1]
  (j,is2) <- removeRndIndex is1
  (k,_)   <- removeRndIndex is2
  return (i,j,k)
  where
  removeRndIndex is = do
    s <- state $ uniformR (0,length is-1)
    let (pre,i:post) = splitAt s is
    return (i,pre++post)
