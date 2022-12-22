{-
Given n finite, disjoint sets with cardinalities c = [c_1, ..., c_n], form the
largest possible set T of triplets by selecting elements from the sets without
replacement. A triplet may not contain multiple elements from the same set.
What's the cardinality of T?

The naivePhases function below underestimates some cases, e.g.
countTriplets (naivePhases [26,26,31,4]) == 26 . For this case, if the sets are
A, B, C and D, you can form 28 triplets as follows:

Phase # |A| # |B| # |C| # |D| # |T|
###################################
   0 #  26 #  26 #  31 #   4 #   0
   1 #   2 #   2 #   7 #   4 #  24
   2 #   0 #   2 #   5 #   2 #  26
   3 #   0 #   0 #   3 #   0 #  28

Let p0 = Problem c, and let ps be the closure of {p0} under subProblems. Let es
be a set of edges between the Problems of ps such that (p,q) is in es if and
only if q is in subProblems p. The pair (ps,es) defines a DAG.

The cardinality of T is equal to the number of edges on the longest path that
starts at p0. This is similar to the Longest Increasing Subsequence problem,
except that there's one starting node and multiple terminal nodes, and it's not
immediately obvious how to enumerate the terminal nodes without computing ps,
which may be massive.
-}

{-# LANGUAGE TupleSections #-}

module Main(main) where

import Data.Char
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Ord
import qualified Data.Set as S
import Text.Printf


newtype Phases = Phases {phases :: [Phase]} deriving(Show)

data Phase = Phase {triplet :: Triplet, count :: Int} deriving(Show)

type Triplet = (Int,Int,Int)

data LowerBound = LowerBound {problem :: Problem, atLeast :: Int} deriving(Show)

newtype Problem = Problem {setSpecs :: S.Set SetSpec} deriving(Eq,Ord,Show)

data SetSpec = SetSpec {cardinality :: Int, index :: Int} deriving(Eq,Ord,Show)


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
  let [i0,i1,i2] = sort [index x, index y, index z] in (i0,i1,i2)


main = do
  csv <- readFile "TrinitySnapshotTest-12_20-12_22.csv"
  let eval = evaluate $ fromJust $ parseLowerBounds 10 csv
  eval "naivePhases" naivePhases
  eval "betterPhases" betterPhases

evaluate :: [(String,LowerBound)] -> String -> (Problem -> Phases) -> IO ()
evaluate wLBs name phases = do
  let (wallets,lbs) = unzip wLBs
  let ps = phases . problem <$> lbs
  let select pred = filter (uncurry pred . snd) $ zip wallets $ zip ps lbs
  let exceeders = select exceedsLowerBound
  let failures = select belowLowerBound

  printHeading $ name ++ " exceeders:"
  putStrLn $ toCSV exceeders

  printHeading $ name ++ " failures:"
  putStrLn $ toCSV failures

printHeading :: String -> IO ()
printHeading heading =
  let line = replicate 80 '='
  in  putStrLn "" >> putStrLn line >> putStrLn heading >> putStrLn line

exceedsLowerBound :: Phases -> LowerBound -> Bool
exceedsLowerBound ps lb = countTriplets ps > atLeast lb

belowLowerBound :: Phases -> LowerBound -> Bool
belowLowerBound ps lb = countTriplets ps < atLeast lb

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
setSpec (card,i) = SetSpec {cardinality=card, index=i}

parseCSV :: String -> [(String,[Maybe Int])]
parseCSV = filter (not . null) . map parseRow . lines

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
    $   "Wallet" : [printf "Set %d" i | i <- setIs]
    ++  ["Old Trinities","New Trinities"]
    ++  [printf "Phase %d" i | i <- phaseIs]

  row (wallet, (ps@(Phases pList), lb)) =
    intercalate ","
    $   wallet : [M.findWithDefault "" i i2CardStr | i <- setIs]
    ++  map show [atLeast lb, countTriplets ps]
    ++  map phaseStr pList
    ++  replicate (maxNoPhases - length pList) ""
    where
    i2CardStr =
      M.fromList
      $   (\sSpec -> (index sSpec, show $ cardinality sSpec))
      <$> (S.toList $ setSpecs $ problem lb)

    phaseStr p = "\"" ++ show (count p) ++ " × " ++ show (triplet p) ++ "\""

  setIs = [0..maxNoSets-1]
  phaseIs = [0..maxNoPhases-1]
  maxNoPhases = maximum $ 0 : map noPhases walletPhasesLBs
  maxNoSets = maximum $ 0 : map noSets walletPhasesLBs
  noPhases = length . phases . fst . snd
  noSets = S.size . setSpecs . problem . snd . snd
