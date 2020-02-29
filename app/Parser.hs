{-# LANGUAGE LambdaCase #-}

module Parser
(  
    runParser,
    parseChar,
    parseString,
    parseBool
) where

import Data.Char
import Control.Applicative

type Error = String

newtype Parser a = Parser { runParser :: String -> Either Error (a, String) }

instance Functor Parser where
    fmap f p = Parser $ \input -> do
        (r, t) <- runParser p $ input
        Right (f r, t)

instance Applicative Parser where
    pure x = Parser $ \input -> Right (x, input)
    p1 <*> p2 = Parser $ \input -> do
        (f, r) <- runParser p1 $ input
        (x, r') <- runParser p2 $ r
        Right (f x, r')

instance Alternative Parser where
    empty = Parser $ \_ -> Left ""
    p1 <|> p2 = Parser $ \input -> do
        case (runParser p1) input of
            Left _ -> (runParser p2) input
            Right (r, t) -> Right (r, t)

parseChar :: Char -> Parser Char
parseChar c = Parser $ 
    \case input@(h:t) -> if h == c then
                            Right (c, t)
                         else 
                            Left $ "Trying to parse character " ++ [c] ++ ", but " ++ input ++ " does not contain " ++ [c] ++ "."  
          [] -> Left $ "Trying to parse character " ++ [c] ++ ", but given empty string." 

parseString :: String -> Parser String
parseString = sequenceA . map parseChar

