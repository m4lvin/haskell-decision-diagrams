module Data.DecisionDiagram.Visuals where

import Data.DecisionDiagram.BDD as BDD
import Data.DecisionDiagram.ZDD as ZDD
import Data.GraphViz
import Data.GraphViz.Types.Generalised
import Data.GraphViz.Types.Monadic
import Data.IntMap hiding (map)
import Data.IntSet as IntSet

-- * Helper functions

labelFor :: Show a => Sig a -> String
labelFor (SLeaf True) = "T"
labelFor (SLeaf False) = "F"
labelFor (SBranch v _ _) = show v

edgesFor :: Int -> Sig Int -> DotM Int ()
edgesFor _ (SLeaf True) = return ()
edgesFor _ (SLeaf False) = return ()
edgesFor n (SBranch _ a b) = do
  edge n a [style dashed] -- false edge is dashed
  edge n b []

restrictToReachable :: (Graph Sig, Int) -> (Graph Sig, Int)
restrictToReachable (g, k) = (restrictKeys g reached, k) where
  reached = IntSet.insert k $ children (g ! k)
  children (SBranch _ a b) = IntSet.unions [ IntSet.fromList [a,b]
                                           , children (g ! a)
                                           , children (g ! b) ]
  children (SLeaf False) = IntSet.fromList [0]
  children (SLeaf True) = IntSet.fromList [1]

-- * BDDs

-- | Use `toGraph` to create a GraphViz representation of a `BDD`.
bddToGraphViz :: BDD a -> Data.GraphViz.Types.Generalised.DotGraph Int
bddToGraphViz b = digraph' $ do
  let g = fst (restrictToReachable $ BDD.toGraph b)
  let fix | b == true = Data.IntMap.delete 0
          | b == false = Data.IntMap.delete 1
          | otherwise = id
  let nodes = (Data.IntMap.keys (fix g) :: [Int])
  mapM_ (\ n -> node n [toLabel $ labelFor $ (Data.IntMap.!) g n]) nodes
  mapM_ (\ n -> edgesFor n ((Data.IntMap.!) g n)) nodes

-- | Write a PDF of the given BDD to the given file name.
bddPdf :: BDD a -> FilePath -> IO FilePath
bddPdf x = runGraphviz (bddToGraphViz x) Pdf

-- | Show Bdd in a window. Might only work on Linux.
bddShow :: BDD a -> IO ()
bddShow x = runGraphvizCanvas Dot (bddToGraphViz x) Xlib

-- * ZDDs

-- | Use `toGraph` to create a GraphViz representation of a `ZDD`.
zddToGraphViz :: ZDD a -> Data.GraphViz.Types.Generalised.DotGraph Int
zddToGraphViz b = digraph' $ do
  let g = fst (restrictToReachable $ ZDD.toGraph b)
  let nodes = (Data.IntMap.keys g :: [Int])
  mapM_ (\ n -> node n [toLabel $ labelFor $ (Data.IntMap.!) g n]) nodes
  mapM_ (\ n -> edgesFor n ((Data.IntMap.!) g n)) nodes

-- | Write a PDF of the given ZDD to the given file name.
zddPdf :: ZDD a -> FilePath -> IO FilePath
zddPdf x = runGraphviz (zddToGraphViz x) Pdf

-- | Show ZDD in a window. Might only work on Linux.
zddShow :: ZDD a -> IO ()
zddShow x = runGraphvizCanvas Dot (zddToGraphViz x) Xlib
