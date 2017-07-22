{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Syntax (Format)
import Parser (parseFormat)
import WordList (parseWordList, WordList)
import Options.Applicative
import Data.Semigroup ((<>))
import System.IO (withFile, IOMode(..), stdin, stdout, Handle)
import Data.Text.IO (hGetContents)
import System.Exit (exitFailure)

data Input =
    FromFile FilePath
    | FromStdin

inputFromStr :: String -> Input
inputFromStr "-" = FromStdin
inputFromStr s = FromFile s

withInput :: Input -> (Handle -> IO a) -> IO a
withInput FromStdin f = f stdin
withInput (FromFile n) f = withFile n ReadMode f

data Output =
    ToFile FilePath
    | ToStdout

outputFromStr :: String -> Output
outputFromStr "-" = ToStdout
outputFromStr s = ToFile s

withOutput :: Output -> (Handle -> IO a) -> IO a
withOutput ToStdout f = f stdout
withOutput (ToFile n) f = withFile n WriteMode f

data Command =
    EncCommand Input
    | DecCommand Input

data FileFormat = AsBase64 | AsBinary

data PoetryOptions = PoetryOptions
    { formatFilename :: FilePath
    , wordListFilename :: FilePath
    , outputFile :: Output
    , outputFormat :: FileFormat
    , commandOpt :: Command }

parseCommand :: Parser Command
parseCommand =
    let parseInput = inputFromStr <$> argument str (metavar "input") in
    subparser
        ( command "enc" (info (EncCommand <$> parseInput) ( progDesc "encode data into poetry" ))
       <> command "dec" (info (DecCommand <$> parseInput) ( progDesc "decode poem into data" )))

parseOpts :: Parser PoetryOptions
parseOpts = PoetryOptions
    <$> strOption
        ( long "format"
       <> short 'f'
       <> metavar "FILENAME"
       <> help "filename for format schema")
    <*> strOption
        ( long "words"
       <> short 'l'
       <> metavar "WORDLIST"
       <> showDefault
       <> value "wordlist.txt"
       <> help "categorized word list")
    <*> (outputFromStr <$> strOption
        ( long "output"
       <> short 'o'
       <> metavar "FILENAME"
       <> showDefault
       <> value "-"
       <> help "filename to output to"))
    <*> flag AsBase64 AsBinary
        ( long "binary"
       <> short 'b'
       <> help "skip base64 encoding/decoding, treat data as binary")
    <*> parseCommand

readFormat :: FilePath -> IO Format
readFormat fname = do
    fmt <- withFile fname ReadMode $ \h ->
        parseFormat fname <$> hGetContents h
    case fmt of
      Left e -> do
        putStrLn "could not parse format"
        print e
        exitFailure
      Right fmt -> return fmt

readWordList :: FilePath -> IO WordList
readWordList fname = do
    wl <- withFile fname ReadMode $ \h ->
        parseWordList fname <$> hGetContents h
    case wl of
        Left e -> do
            putStrLn "could not parse word list"
            print e
            exitFailure
        Right wl -> return wl

mainOp :: PoetryOptions -> IO ()
mainOp PoetryOptions{formatFilename, wordListFilename} = do
    fmt <- readFormat formatFilename
    wl <- readWordList wordListFilename
    putStrLn "format:"
    print fmt
    putStrLn ""
    putStrLn "word list:"
    print wl

main :: IO ()
main = execParser opts >>= mainOp
    where opts = info (parseOpts <**> helper)
            ( fullDesc
           <> progDesc "Encode and decode using a poem format"
           <> header "entropic-poetry: using a poem as an encoding")
