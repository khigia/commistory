module Commistory.GitParser where

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

data GitLogCommit = GitLogCommit { timestamp :: String -- TODO
                                 , author :: String
                                 , changes :: [GitLogChange]
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

parseGitLog :: String -> Either ParseError [(String,String,[String])]
parseGitLog input =
  parse p_commits "(unknown)" input
  where
    p_commits = sepBy p_commit p_nul
    p_commit = do timestamp <- p_timestamp
                  char ' '
                  author <- p_author
                  char '\n'
                  files <- p_files
                  return (timestamp, author, files)
    p_timestamp = many1 digit
    p_author = many1 (noneOf "\n")
    p_files = many (p_binary <|> p_text)
    p_binary = do p_dash
                  p_tab
                  p_dash
                  p_tab
                  f1 <- p_filename
                  p_nul
                  return $ "binary" ++ f1
    p_text = do p_count
                p_tab
                p_count
                p_tab
                p_file
    p_file = p_rename <|> p_change
    p_rename = do p_nul
                  f1 <- p_filename
                  p_nul
                  p_filename
                  p_nul
                  return $ "rename" ++ f1
    p_change = do f1 <- p_filename
                  p_nul
                  return $ "file" ++ f1
    p_nul = char '\0'
    p_tab = char '\t'
    p_dash = char '-'
    p_count = do c <- many1 digit
                 return $ (read c :: Integer)
    p_filename = many (noneOf "\0")
