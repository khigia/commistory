module Commistory.ReportGen (report) where

import qualified Data.ByteString.Lazy as B
import Text.StringTemplate

import Commistory.FsTree

report fsroot = do
  -- TODO create initial folder structure
  templates <- directoryGroup "templates/" :: IO (STGroup B.ByteString)
  -- TODO generate static site frame
  let Just t = getStringTemplate "index.html" templates
  B.writeFile "genweb/index.html" $ render t
  -- TODO generate code report: sunburst of file sizes, file history, line history
  writeFile "genweb/data/fs.json" $ show fsroot
  -- TODO generate activity report: history per commit
  -- TODO generate author author: history per author
