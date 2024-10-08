import System.Environment
import System.Directory
import System.Random
import System.IO
import Data.List as L
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Text.Read
import Text.Printf 


-- LeftistHeap that stores a triple (Int, [[Int]], [[[Int]]]) for (value, current_board, 
-- path_to_current_board) respectively. Value is the fScore of the node (state of the board). 
--  * remove_smallest removes and returns a node on the queue with the lowest fScore value. 
--  * fScore[n] represents our current best guess as to how cheap a path could be 
--    from start to finish if it goes through n.

data LeftistHeap a = Nil | Node  (LeftistHeap a) a (LeftistHeap a)
    deriving Show

rank :: LeftistHeap a -> Int
rank Nil = 0
rank (Node _ _ right) = 1 + rank right

merge :: Ord a => LeftistHeap a -> LeftistHeap a -> LeftistHeap a
merge Nil h = h
merge h Nil = h
merge h@(Node left x right) h'@(Node left' x' right')
    | x' < x = merge h' h
    | x' == x = h
    | otherwise =
        let h1 = merge right h'
        in if rank left >= rank h1 then Node left x h1 else Node h1 x left

insertHeap :: Ord a => a -> LeftistHeap a -> LeftistHeap a
insertHeap x h = merge (Node Nil x Nil) h

remove_smallest :: Ord a => LeftistHeap a -> (a, LeftistHeap a)
remove_smallest (Node left x right) = (x, merge left right)



type MyTriple =  (Int, [[Int]], [[[Int]]])

data Parameters = Parameters {
                  gScore :: Map [[Int]] Int,
                  minHeap :: LeftistHeap MyTriple}
  deriving(Show)

type State = [[Int]]
  
--------------------------------------------------------------------------------------------------------

-- convert a list (concatenated board) to a board (list of list of numbers)
listToBoard :: (Ord a) => [a] -> [[a]]
listToBoard [] = []
listToBoard arr = r1:r2:r3:[r4]
  where (h1, h2) = L.splitAt 8 arr
        (r1, r2) = L.splitAt 4 h1
        (r3, r4) = L.splitAt 4 h2 

edgeCase :: Int -> Int -> Bool 
edgeCase i1 i2 = m1 == 0 && m2 == 3 || m1 == 3 && m2 == 0
  where m1 = i1 `mod` 4
        m2 = i2 `mod` 4

-- swap the elements of the list at the given indices
swapAtIndices :: [a] -> Int -> Int -> Maybe [a]
swapAtIndices arr i1 i2 
    -- check that the indices are not out of bounds or edge cases
  | i1>=0  && i1<=15 && i2>=0 && i2<=15 && not (edgeCase i1 i2) = Just updatedArr      
  | otherwise = Nothing
  where (t1,(el1:t2)) = L.splitAt i1 arr
        (s1, (el2:s2)) = L.splitAt i2 arr1 
        arr1 = t1 ++ (el2:t2)
        updatedArr = s1 ++ (el1:s2)

--------------------------------------------------------------------------------------------------------

isGoalState :: State -> Bool
isGoalState board = board == goalState

goalState :: State
goalState = [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12], [13, 14, 15, 0]]


getNeighbors :: State -> [State]
getNeighbors current = map listToBoard validNeighbors
  where
    concatenated = concat current
    currIndex = fromJust (0 `elemIndex` concatenated)
    neighbors = [swapAtIndices concatenated currIndex (currIndex + i) | i <- [-4, -1, 4, 1]]
    validNeighbors = catMaybes neighbors


-- heuristic N1
manhattanDistances :: State -> [Int]
manhattanDistances board = L.map (getManhattan board) (concat board)

getManhattan :: State -> Int -> Int
getManhattan board val  
  | val == 0 = abs (x1 - 3) + abs (y1 - 3)
  | otherwise = abs (x1 - x2) + abs (y1 - y2)
  where
    -- x1, y1 = current coordinates of the number
    -- x2, y2 = where they should be
    arr = concat board
    (x1, y1) = divMod (fromJust (val `elemIndex` arr)) 4
    (x2, y2) = divMod (val - 1) 4


exploreNeighbor :: State -> [State] -> Parameters -> State -> Parameters
exploreNeighbor current prev params@(Parameters gScore minHeap) neighbor 
-- tentative_gScore is the distance from start to the neighbor through current
-- This path to neighbor is better than any previous one, so we modify the parameters accordingly
  | tentative_gScore < neighGScore = Parameters updatedgSc updatedminHeap
  | otherwise = params
  where tentative_gScore = fromJust (Map.lookup current gScore) + 1         
                                -- 1 is the weight of the edge from current to neighbor
        neighGScore = Map.findWithDefault maxBound neighbor gScore
        updatedgSc = Map.insert neighbor tentative_gScore gScore  
        heuristic = sum (manhattanDistances neighbor)
        fSc = tentative_gScore + heuristic

        p' = neighbor:prev
        triple = (fSc, neighbor, p')
        updatedminHeap = insertHeap triple minHeap

aStar :: State -> Parameters -> Int -> ([State], Int) 
aStar _  (Parameters _ Nil) count = ([], count)       -- running aStar before the minQueue is empty.
    -- if this ever happens, we return an empty list,
    -- since the minQueue is empty but goal was never reached.
aStar start (Parameters gScore minHeap) count
  | isGoalState current = (reverse prev, count)
  | otherwise = (aStar start (Parameters gSc' minH') (count + 1))
    where (triple, updatedMinHeap) = remove_smallest minHeap
          (val, current, prev) = triple
          neighbors = getNeighbors current
          (Parameters gSc' minH') = foldl (exploreNeighbor current prev) (Parameters gScore updatedMinHeap) neighbors


--------------------------------------------------------------------------------------------------------

randomPuzzle :: Int -> IO State
randomPuzzle n = do
    gen <- newStdGen
    let f (p, gen) =
            let ns = getNeighbors p
                (i, gen') = randomR (0, length ns - 1) gen
            in (ns !! i, gen')
    return $ fst ((iterate f (goalState, gen)) !! n)

printMatrix :: [[Int]] -> String
printMatrix matrix = unlines $ map printRow matrix
  where
    maxDigits = maximum $ map (maximum . map (length . show)) matrix
    printRow row = concatMap (printCell maxDigits) row
    printCell digits x = printf ("%*d " :: String) digits x

printList :: [State] -> IO ()
printList ms = putStr (unlines (L.map printMatrix ms))


solve :: State -> IO ()
solve board = do
  let gScore =  Map.fromList [(board,0)]   
      s = sum (manhattanDistances board)  
      minHeap = insertHeap (s, board, [board]) Nil      
      params = Parameters gScore minHeap
      (result, count) = aStar board params 0
      moves = length result
      (initial:rest) = result
  putStrLn "The initial state:"
  printList [initial]
  putStrLn "Optimal solution:"
  printList rest
  putStrLn $ "Number of explored states: " ++ show count
  putStrLn $ "Number of moves: " ++ show moves

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do 
      isFile <- doesFileExist filename
      if isFile 
        then do 
          contents <- readFile filename
          let linesOfFile = lines contents 
              board = L.map (L.map read . words) linesOfFile :: [[Int]]
          solve board
      else 
        putStrLn $ "File " ++ show filename ++ " does not exist"
    ["-r", num] -> do 
      case reads num of 
        [(m, "")] -> do
          let n = m :: Int 
          board <- randomPuzzle n
          solve board
        _ -> putStrLn "usage: runghc 15-solver.hs [<filename> | -r <num-random-moves>]"  
    _ -> putStrLn "usage: runghc 15-solver.hs [<filename> | -r <num-random-moves>]"