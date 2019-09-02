module TopSort where
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Control.Monad.State.Lazy

-- Type of the vertex of the graph.
type Vertex = String

-- The DFS state is a pair of vertex lists: (dfs stack vertices, already visited vertices).
type DfsState = ([Vertex], [Vertex])

{- Given a map from vertices to it's neighbors representing the directed graph.
   Returns a list with a cycle (if any exists) -}
existsCycle :: (M.Map Vertex [Vertex]) -> (Maybe [Vertex])
existsCycle suc = fst $ runState (liftM firstJust . mapM (eC . fst) . M.toList $ suc) ([],[])
  where eC :: Vertex -> State DfsState (Maybe [Vertex])
        eC v = do (ancestors, visited) <- get
                  let newAncestors = v:ancestors
                      newVisited   = v:visited
                      isBackEdge x =  if elem x ancestors then return . Just $ makeCycle x newAncestors
                                        else if elem x visited then return Nothing
                                        else eC x
                  put (newAncestors, newVisited)
                  returnV <- liftM firstJust $ forM (M.findWithDefault [] v suc) isBackEdge
                  put (ancestors, newVisited)
                  return returnV
        makeCycle x s = reverse $ x : take (((+1) . fromJust . elemIndex x) s) s
        firstJust = join . find isJust
