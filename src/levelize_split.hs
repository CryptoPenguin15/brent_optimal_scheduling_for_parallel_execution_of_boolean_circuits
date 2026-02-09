{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (fromMaybe)
import Data.Bits

data Gate = Gate { gInputs :: [Int], gOutput :: Int, gOp :: T.Text, gLine :: T.Text }
  deriving (Show, Eq)

parseGate :: T.Text -> Gate
parseGate line =
  let tok = T.words line
      nin = read (T.unpack (tok !! 0)) :: Int
      ins = map (read . T.unpack) (take nin (drop 2 tok))
      out = read (T.unpack (tok !! (2 + nin))) :: Int
      op  = last tok
  in Gate ins out op line

-- Depth, levelization, with custom base-depth wires
buildInAdj :: [Gate] -> M.Map Int [Int]
buildInAdj gs = M.fromList [(gOutput g, gInputs g) | g <- gs]

nodeDepth :: S.Set Int -> M.Map Int [Int] -> Int -> M.Map Int Int -> (Int, M.Map Int Int)
nodeDepth base inAdj w memo
  | S.member w base = (0, M.insert w 0 memo)
  | otherwise =
      case M.lookup w memo of
        Just d  -> (d, memo)
        Nothing ->
          case M.lookup w inAdj of
            Nothing -> (0, M.insert w 0 memo) -- true inputs/constants
            Just ps ->
              let (ds,m') = foldl (\(acc,m) p ->
                                    let (d,m2) = nodeDepth base inAdj p m
                                    in (d:acc,m2))
                                 ([],memo) ps
                  d = 1 + maximum ds
              in (d, M.insert w d m')

computeDepths :: S.Set Int -> [Gate] -> M.Map Int Int
computeDepths base gs =
  let inAdj = buildInAdj gs
  in snd $ foldl (\(_,m) w -> nodeDepth base inAdj w m) (0,M.empty) (M.keys inAdj)

levelize :: S.Set Int -> [Gate] -> [Gate]
levelize base gs =
  let dm = computeDepths base gs
      d g = fromMaybe 0 (M.lookup (gOutput g) dm)
  in L.sortOn d gs

-- Taint
keyMask, dataMask :: Int
keyMask = 1; dataMask = 2

initTaint :: M.Map Int Int
initTaint = M.fromList ([(i,keyMask) | i <- [0..127]] ++ [(i,dataMask) | i <- [128..255]])

propTaint :: M.Map Int Int -> Gate -> M.Map Int Int
propTaint dep g =
  let t = foldl (\acc w -> acc .|. M.findWithDefault 0 w dep) 0 (gInputs g)
  in M.insert (gOutput g) t dep

isKOnly, isMixed :: Int -> Bool
isKOnly t = t == keyMask
isMixed t = (t .&. keyMask /= 0) && (t .&. dataMask /= 0)

-- reverse edges out->ins for slicing
revEdges :: [Gate] -> M.Map Int [Int]
revEdges gs = M.fromList [(gOutput g, gInputs g) | g <- gs]

-- frontier K wires: K-only wires that feed a mixed-output gate
frontierKWires :: M.Map Int Int -> [Gate] -> S.Set Int
frontierKWires dep =
  foldl step S.empty
  where
    step acc g =
      let tout = M.findWithDefault 0 (gOutput g) dep
      in if isMixed tout
           then foldl (\a w -> if isKOnly (M.findWithDefault 0 w dep) then S.insert w a else a)
                      acc (gInputs g)
           else acc

-- backward slice restricted to K-only wires
sliceKOnly :: M.Map Int Int -> M.Map Int [Int] -> S.Set Int -> S.Set Int
sliceKOnly dep parents seeds = go S.empty (S.toList seeds)
  where
    go seen [] = seen
    go seen (w:ws)
      | S.member w seen = go seen ws
      | not (isKOnly (M.findWithDefault 0 w dep)) = go seen ws
      | otherwise =
          let ps = M.findWithDefault [] w parents
          in go (S.insert w seen) (ps ++ ws)

main :: IO ()
main = do
  let infile  = "../input/aes_128_mand.txt"
      outfile = "../output/aes_128_mand_split.txt"

  content <- TIO.readFile infile
  let ls = T.lines content
      header = take 3 ls
      gateLines = filter (not . T.null . T.strip) (drop 3 ls)
      gates0 = map parseGate gateLines

  -- IMPORTANT: taint on original, Bristol, topo order
  let taint0   = foldl propTaint initTaint gates0
      parents  = revEdges gates0
      frontK   = frontierKWires taint0 gates0
      kConeW   = sliceKOnly taint0 parents frontK

      isKeyGate g = isKOnly (M.findWithDefault 0 (gOutput g) taint0) && S.member (gOutput g) kConeW

      keyGates  = filter isKeyGate gates0
      restGates = filter (not . isKeyGate) gates0

      -- levelize key part with base, key and data inputs
      baseInputs = S.fromList [0..255]
      keyLev  = levelize baseInputs keyGates

      -- for rest part, treat key-cone outputs as available at start too
      keyOuts = S.fromList (map gOutput keyGates)
      restBase = baseInputs `S.union` keyOuts
      restLev = levelize restBase restGates

      finalGates = keyLev ++ restLev
      splitIdx = length keyLev - 1

  TIO.writeFile outfile $ T.unlines $ header ++ [""] ++ map gLine finalGates

  putStrLn "Split+levelized file written."
  putStrLn $ "Key expansion ends at gate index: " ++ show splitIdx
  putStrLn $ "Total gates: " ++ show (length finalGates)
  putStrLn $ "Key gates: " ++ show (length keyLev) ++ ", Rest gates: " ++ show (length restLev)
