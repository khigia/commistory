module Commistory.ReportGen (report) where

import qualified Data.ByteString.Lazy as B
import Text.StringTemplate
import Data.List (intercalate)

import Commistory.FsTree

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

report fsroot = do
  -- TODO create initial folder structure
  -- TODO generate static site frame
  templates <- directoryGroup "templates/" :: IO (STGroup B.ByteString)
  let Just base = getStringTemplate "base.html" templates
  let navbase active = setAttribute "navitems" (genNav active) base
  B.writeFile "genweb/index.html" $ render
                                  $ setAttribute "content" "general"
                                  $ navbase "General"
  -- TODO generate code report: sunburst of file sizes, file history, line history
  let Just code = getStringTemplate "code.html" templates
      codecontent = render code
  B.writeFile "genweb/code.html" $ render
                                 $ setAttribute "content" codecontent
                                 $ navbase "Code"
  writeFile "genweb/data/fs.json" $ show fsroot
  -- TODO generate activity report: history per commit
  B.writeFile "genweb/activity.html" $ render
                                     $ setAttribute "content" "activity"
                                     $ navbase "Activity"
  -- TODO generate author author: history per author
  B.writeFile "genweb/authors.html" $ render
                                    $ setAttribute "content" "authors"
                                    $ navbase "Authors"
