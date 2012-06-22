module Commistory.FsTree where

import qualified Data.Map as Map
import Data.List (intercalate)
import System.FilePath (splitDirectories)

data Fs = FsNode { fsnName:: String
                 , fsnSize :: Integer
                 , fsnChildren :: Map.Map String Fs
                 }

fileNode name sz = FsNode{ fsnName = name
                         , fsnSize = sz
                         , fsnChildren = Map.empty
                         }

dirNode name = FsNode{ fsnName = name
                     , fsnSize = 0
                     , fsnChildren = Map.empty
                     }

dirNodeWithChildren name childrenFs =
  FsNode { fsnName = name
         , fsnSize = size
         , fsnChildren = Map.fromList children
         }
  where
    children = map (\c -> (fsnName c, c)) childrenFs
    size = foldl (+) 0 (map fsnSize childrenFs)

instance Show Fs where
  -- TODO can use aeson library? or at least StringTemplate
    show n | Map.null (fsnChildren n) =
                    "{" ++ "\"name\": \"" ++ (fsnName n)
                        ++ "\",\"size\": " ++ (show $ fsnSize n)
                        ++ "}"
           | otherwise =
                    "{" ++ "\"name\": \"" ++ (fsnName n)
                        ++ "\",\"children\": ["
                               ++ (showchildren $ fsnChildren n)
                           ++ "]"
                        ++ "}"
               where showchildren ns =
                        intercalate "," (map show (Map.elems ns))

addPath root path sz =
  addPath' root comps filename sz
  where
    spmoc = reverse $ splitDirectories path
    filename = head spmoc
    comps = reverse $ tail spmoc
    addPath' root' comps' fn' sz'
      | null comps' = root' { fsnSize = sz' + fsnSize root'
                            , fsnChildren = Map.insert fn' (fileNode fn' sz') (fsnChildren root')
                            }
      | otherwise = case Map.lookup (head comps') (fsnChildren root') of
                    Just e -> root' { fsnSize = sz' + fsnSize root'
                                    , fsnChildren = Map.insert (head comps') newChild (fsnChildren root')
                                    }
                              where newChild = addPath' e (tail comps') fn' sz'
                    Nothing -> let child0 = dirNode (head comps')
                                   child1 = addPath' child0 (tail comps') fn' sz'
                               in root' { fsnSize = sz' + fsnSize root'
                                        , fsnChildren = Map.insert (head comps') child1 (fsnChildren root')
                                        }

