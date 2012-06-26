module Commistory.ReportGen (report) where

import qualified Data.ByteString.Lazy as B
import Text.StringTemplate
import Data.List (intercalate, nub)
import Data.Text (unpack)
import Data.Time (utctDay)

import Commistory.FsTree
import Commistory.GitParser -- TODO move datastruct outside of parser
import Commistory.Config

genNav active = intercalate "\n" navitems
  where
    navitems = map navitem [ ("index.html", "General")
                           , ("activity.html", "Activity")
                           , ("authors.html", "Authors")
                           , ("code.html", "Code")
                           ]
    navitem (url, title)
      | title == active = baseitem " class=\"active\"" url title
      | otherwise       = baseitem "" url title
    baseitem cls url title = render
                             $ setAttribute "cls" cls
                             $ setAttribute "url" url
                             $ setAttribute "title" title
                             templ
    templ = newSTMP "<li$cls$><a href=\"$url$\">$title$</a></li>" :: StringTemplate String

data StatRepo = StatRepo { stName :: String
                         , stAuthors :: Integer
                         , stCommits :: Integer
                         , stActiveDays :: Integer
                         , stFiles :: Integer
                         , stSize :: Integer
                         , stAddLines :: Integer
                         , stDelLines :: Integer
                         }
genGeneralTable config fstrees allcommits = intercalate "\n" renderedRows
  where
    renderedRows = map renderRow repoRows
    repoRows = map repoRow $ zip3 (cfgRepositories config) fstrees allcommits
    repoRow (cfg, fstree, commits) =
      StatRepo { stName = unpack $ repoName cfg
               , stAuthors = fromIntegral . length . nub $ map ciAuthor commits
               , stCommits = fromIntegral $ length commits
               , stActiveDays = fromIntegral . length . nub $ map (utctDay . ciTimestamp) commits
               , stFiles = fileCount fstree
               , stSize = fsnSize fstree
               , stAddLines = addLines
               , stDelLines = delLines
               }
      where (addLines, delLines) = foldr padd (0,0) $ map countCommitLines commits
    renderRow stats =
      render
      $ setAttribute "repo" (stName stats)
      $ setAttribute "authors" (show $ stAuthors stats)
      $ setAttribute "commits" (show $ stCommits stats)
      $ setAttribute "activedays" (show $ stActiveDays stats)
      $ setAttribute "files" (show $ stFiles stats)
      $ setAttribute "size" (show $ stSize stats)
      $ setAttribute "addlines" (show $ stAddLines stats)
      $ setAttribute "dellines" (show $ stDelLines stats)
      templ
    templ = newSTMP "<tr><td>$repo$</td><td>$authors$</td><td>$commits$</td><td>$activedays$</td><td>$files$</td><td>$size$</td><td>$addlines$</td><td>$dellines$</td></tr>" :: StringTemplate String

report config fstrees commits = do
  -- TODO create initial folder structure
  -- TODO generate static site frame
  templates <- directoryGroup "templates/" :: IO (STGroup B.ByteString)
  let Just base = getStringTemplate "base.html" templates
  let navbase active = setAttribute "navitems" (genNav active) base
  -- TODO generate general detail as main page
  let Just generaltmpl = getStringTemplate "general.html" templates
      general = render $ setAttribute "tbody" (genGeneralTable config fstrees commits)
                       generaltmpl
  B.writeFile "genweb/index.html" $ render
                                  $ setAttribute "content" general
                                  $ navbase "General"
  -- TODO generate code report: sunburst of file sizes, file history, line history
  let Just codetmpl = getStringTemplate "code.html" templates
      code = render codetmpl
  B.writeFile "genweb/code.html" $ render
                                 $ setAttribute "content" code
                                 $ navbase "Code"
  let fsroot = dirNodeWithChildren (unpack $ cfgProjectName config) fstrees
  writeFile "genweb/data/fs.json" $ show fsroot
  -- TODO generate activity report: history per commit
  let Just activitytmpl = getStringTemplate "activity.html" templates
      activity = render activitytmpl
  B.writeFile "genweb/activity.html" $ render
                                     $ setAttribute "content" activity
                                     $ navbase "Activity"
  -- TODO generate author author: history per author
  let Just authorstmpl = getStringTemplate "authors.html" templates
      authors = render authorstmpl
  B.writeFile "genweb/authors.html" $ render
                                    $ setAttribute "content" authors
                                    $ navbase "Authors"
