module Parsing where

import Text.Parsec

type Parser a = Parsec String () a

optimisticInteract :: Parser a -> (a -> String) -> IO ()
optimisticInteract parser solver = do
  interact interacter
  putStrLn ""
  where
    interacter str = case parse parser "" str of
      Left err -> show err
      Right p  -> solver p

mnumber :: Parser Integer
mnumber = do
  m <- option "" $ string "-"
  n <- many1 digit
  return $ read (m ++ n)

number :: Parser Integer
number = do
  n <- many1 digit
  return $ read n

digitAsNumber :: Parser Integer
digitAsNumber = do
  n <- count 1 digit
  return $ read n

word :: Parser String
word = many1 letter
