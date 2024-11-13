module Data.DecisionDiagram.BDD.Visuals where

import Data.DecisionDiagram.BDD
import Data.GraphViz
import Data.GraphViz.Types.Generalised
import Data.GraphViz.Types.Monadic
import Data.IntMap hiding (map)

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

-- | Use `toGraph` to create a GraphViz representation of a `BDD`.
ddToGraphViz :: BDD a -> Data.GraphViz.Types.Generalised.DotGraph Int
ddToGraphViz b = digraph' $ do
  let g = fst (toGraph b)
  let fix | b == true = delete 0
          | b == false = delete 1
          | otherwise = id
  let nodes = (Data.IntMap.keys (fix g) :: [Int])
  mapM_ (\ n -> node n [toLabel $ labelFor $ (Data.IntMap.!) g n]) nodes
  mapM_ (\ n -> edgesFor n ((Data.IntMap.!) g n)) nodes

-- | Write the picture of given Bdd to "example.pdf"
pdfBdd :: BDD a -> IO FilePath
pdfBdd x = runGraphviz (ddToGraphViz x) Pdf "example.pdf"

-- | Show Bdd in a window. Might only work on Linux.
showBdd :: BDD a -> IO ()
showBdd x = runGraphvizCanvas Dot (ddToGraphViz x) Xlib
