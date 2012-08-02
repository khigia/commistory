module Commistory.ReportGen (report) where

import qualified Data.ByteString.Lazy as B
import Text.StringTemplate
import Data.List (intercalate, nub)
import Data.Text (unpack)
import Data.Time (UTCTime, utctDay, getZonedTime)
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime)
import Data.Time.Format (formatTime)
import Locale (defaultTimeLocale)

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


commitsToJson tz repocommits =
  render $ setAttribute "repocommits" renderedRepos $ tpl
  where
    renderedRepos = intercalate "," $ map renderRepo repocommits
    renderRepo (repo, commits) =
      render
      $ setAttribute "repo" (repo)
      $ setAttribute "commits" (renderCommits commits)
      tplRepo
    renderCommits commits = intercalate "," $ map renderCommit commits
    renderCommit ci = render
                    $ setAttribute "timestamp" (renderTime $ ciTimestamp ci)
                    $ setAttribute "author" (show $ ciAuthor ci)
                    $ setAttribute "la" (show $ fst lines)
                    $ setAttribute "ld" (show $ snd lines)
                    tplCommit
      where lines = countCommitLines ci
    renderTime t = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" $ utcToLocalTime tz t
    tpl = newSTMP "[$repocommits$]" :: StringTemplate String
    tplRepo = newSTMP "{\"repo\":\"$repo$\",\"commits\":[$commits$]}" :: StringTemplate String
    tplCommit = newSTMP "{\"stamp\":\"$timestamp$\",\"author\":$author$,\"la\":$la$,\"ld\":$ld$}" :: StringTemplate String

report config fstrees commits = do
  -- TODO generate static site frame
  now <- getZonedTime
  tz <- getCurrentTimeZone
  templates <- directoryGroup "templates/" :: IO (STGroup B.ByteString)
  let Just base = getStringTemplate "base.html" templates

  let navbase active = setAttribute "navitems" (genNav active) base
  let Just generaltmpl = getStringTemplate "general.html" templates
      general = render $ setAttribute "tbody" (genGeneralTable config fstrees commits)
                       $ setAttribute "genstamp" now
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
  writeFile "genweb/data/commits.json"
    $ commitsToJson tz
    $ zip (map repoName $ cfgRepositories config) commits
  -- TODO generate author author: history per author
  let Just authorstmpl = getStringTemplate "authors.html" templates
      authors = render authorstmpl
  B.writeFile "genweb/authors.html" $ render
                                    $ setAttribute "content" authors
                                    $ navbase "Authors"
