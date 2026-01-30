{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (fromMaybe)

-- Gate type
data Gate = Gate
  { gInputs :: [Int]
  , gOutput :: Int
  , gOp     :: T.Text
  , gLine   :: T.Text
  } deriving (Show, Eq)

-- Parse one gate line
parseGate :: T.Text -> Gate
parseGate line =
  let tok  = T.words line
      nin  = read (T.unpack (tok !! 0)) :: Int
      ins  = map (read . T.unpack) (take nin (drop 2 tok))
      out  = read (T.unpack (tok !! (2 + nin))) :: Int
      op   = last tok
  in Gate ins out op line

-- Build reverse adjacency: node -> its inputs
buildInAdj :: [Gate] -> M.Map Int [Int]
buildInAdj gates = M.fromList [(gOutput g, gInputs g) | g <- gates]

-- Depth computation, memoized
nodeDepth :: M.Map Int [Int] -> Int -> M.Map Int Int -> (Int, M.Map Int Int)
nodeDepth inAdj w memo =
  case M.lookup w memo of
    Just d  -> (d, memo)
    Nothing ->
      case M.lookup w inAdj of
        Nothing -> (0, M.insert w 0 memo)
        Just parents ->
          let (depths, memo') = foldl
                (\(ds,m) p ->
                  let (d,m2) = nodeDepth inAdj p m
                  in (d:ds, m2))
                ([], memo) parents
              d = 1 + maximum depths
          in (d, M.insert w d memo')

computeAllDepths :: M.Map Int [Int] -> M.Map Int Int
computeAllDepths inAdj =
  let nodes = M.keys inAdj
  in snd $ foldl
       (\(_,memo) n -> nodeDepth inAdj n memo)
       (0, M.empty) nodes

-- Main
main :: IO ()
main = do
  let infile  = "../input/aes_128_mand.txt"
      outfile = "../output/aes_128_mand.txt"

  content <- TIO.readFile infile
  let ls = T.lines content

  -- header
  let header = take 3 ls
      rawGateLines = drop 3 ls
      gateLines = filter (not . T.null . T.strip) rawGateLines

  -- parse gates
  let gates = map parseGate gateLines

  -- build graph
  let inAdj = buildInAdj gates

  -- compute depths
  let depthMap = computeAllDepths inAdj
      getDepth g = fromMaybe 0 (M.lookup (gOutput g) depthMap)

  -- sort by level
  let orderedGates = L.sortOn getDepth gates

  -- write output
  TIO.writeFile outfile $
    T.unlines $ header ++ [""] ++ map gLine orderedGates

  putStrLn "Levelized file written."
