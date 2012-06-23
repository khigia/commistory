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
  -- TODO generate general detail as main page
  let Just generaltmpl = getStringTemplate "general.html" templates
      general = render generaltmpl
  B.writeFile "genweb/index.html" $ render
                                  $ setAttribute "content" general
                                  $ navbase "General"
  -- TODO generate code report: sunburst of file sizes, file history, line history
  let Just codetmpl = getStringTemplate "code.html" templates
      code = render codetmpl
  B.writeFile "genweb/code.html" $ render
                                 $ setAttribute "content" code
                                 $ navbase "Code"
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
