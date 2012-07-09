module Commistory.GitParser where

import Data.Time.Format (readTime)
import Data.Time (UTCTime)
import Locale (defaultTimeLocale)
import Text.ParserCombinators.Parsec

parseGitLsTree :: String -> Either ParseError [(FilePath,Integer)]
parseGitLsTree input =
  parse p_ls "(unknown)" input
  where
    seps = " \t\n"
    p_eol = char '\n'
    p_ls = endBy p_line p_eol
    p_sep = many (oneOf seps)
    p_something = many (noneOf seps)
    p_line = do p_something
                p_sep
                p_something
                p_sep
                many1 hexDigit -- sha1
                p_sep
                size <- many1 digit
                p_sep
                path <- many (noneOf "\n")
                return (path, read size)

-- TODO move these to some data module

data GitLogCommit = GitLogCommit { ciTimestamp :: UTCTime
                                 , ciAuthor :: String
                                 , ciChanges :: [GitLogChange]
                                 }
                  deriving (Show)

data GitLogChange = Binary String
                  | Text { add :: Integer
                         , del :: Integer
                         , name :: GitLogFilename
                         }
                  deriving (Show)

data GitLogFilename = Simple String
                    | Rename String String
                    deriving (Show)

padd p1 p2 = (fst p1 + fst p2, snd p1 + snd p2)

countCommitLines ci = foldr padd (0,0) changeLines
  where
    changeLines = map countChangeLines $ ciChanges ci
    countChangeLines ch = case ch of
      Binary _ -> (0, 0)
      Text a d _ -> (a, d)


parseGitLog :: String -> Either ParseError [GitLogCommit]
parseGitLog input =
  parse p_commits "(unknown)" input
  where
    p_commits = sepBy p_commit p_nul
    p_commit = do timestamp <- p_timestamp
                  char ' '
                  author <- p_author
                  ( p_nul >> p_commit) <|> do char '\n'
                                              files <- p_files
                                              return $ GitLogCommit timestamp author files
    p_timestamp = do stamp <- many1 digit
                     return $ readTime defaultTimeLocale "%s" stamp
    p_author = many1 (noneOf "\n\0")
    p_files = many (p_binary <|> p_text)
    p_binary = do p_dash
                  p_tab
                  p_dash
                  p_tab
                  f1 <- p_filename
                  p_nul
                  return $ Binary f1
    p_text = do added <- p_count
                p_tab
                deleted <- p_count
                p_tab
                name <- p_file
                return $ Text added deleted name
    p_file = p_rename <|> p_change
    p_rename = do p_nul
                  f1 <- p_filename
                  p_nul
                  f2 <- p_filename
                  p_nul
                  return $ Rename f1 f2
    p_change = do f1 <- p_filename
                  p_nul
                  return $ Simple f1
    p_nul = char '\0'
    p_tab = char '\t'
    p_dash = char '-'
    p_count = do c <- many1 digit
                 return $ (read c :: Integer)
    p_filename = many (noneOf "\0")
