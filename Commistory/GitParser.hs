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


