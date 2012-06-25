module Main where

import System.Process
import GHC.IO.Handle
import List (isPrefixOf)
import Data.List (foldl')
import Data.Text (unpack)

import Commistory.FsTree
import Commistory.GitParser
import Commistory.ReportGen
import Commistory.Config


gitLsTree fsroot repo filterpred = do
  -- TODO use -z
  (_, Just hstdout, _, _hpp) <- createProcess (shell "git ls-tree -r -l HEAD") {
      cwd = Just repo,
      std_out = CreatePipe}
  c <- hGetContents hstdout
  -- TODO filter files at parsing time?
  let Right parsed = parseGitLsTree c
  -- TODO building the tree this way is very heavy!
  -- probably sorting file and building dir by dir is faster
  let fstree = foldl' fs_add' fsroot parsed'
      fs_add' n (path, size) = addPath n path size
      parsed' = filter filterpred parsed
  -- TODO ??? waitForProcess hpp
  return fstree

repoFsTree repoCfg = do gitLsTree rootNode path pred
  where
    rootNode = dirNode $ unpack $ repoName repoCfg
    path = unpack (repoPath repoCfg)
    -- TODO build a prefix tree is faster?
    pred n = not (any (\p -> p `isPrefixOf` (fst n)) (repoFileFilterOutPrefix repoCfg))


gitLog repo = do
  (_, Just hstdout, _, _hpp) <- createProcess (shell "git log -z --numstat -M -C --ignore-submodules --pretty=format:\"%at %aN\" --date-order HEAD") {
      cwd = Just repo,
      std_out = CreatePipe}
  c <- hGetContents hstdout
  case parseGitLog c of
    Left e -> print e >>
              return []
    Right parsed -> return parsed
  -- TODO ??? waitForProcess hpp

repoCommits repoCfg = do gitLog path
  where
    path = unpack (repoPath repoCfg)

main :: IO ()
main = do
  -- TODO config file name from args
  Just cfg <- Commistory.Config.readFile "commistory.json"

  -- parse git data
  fstrees <- mapM repoFsTree $ cfgRepositories cfg
  let fsroot = dirNodeWithChildren (unpack $ cfgProjectName cfg) fstrees
  commits <- mapM repoCommits $ cfgRepositories cfg
  print commits

  -- generate report
  report fsroot

