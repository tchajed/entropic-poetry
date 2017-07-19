{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Syntax (Document)
import Parser (parseFormat)
import Options.Applicative
import Data.Semigroup ((<>))
import System.IO (withFile, IOMode(..))
import Data.Text.IO (hGetContents)
import System.Exit (exitFailure)

data FileFormat = AsBase64 | AsBinary

data Command =
    EncCommand FilePath
    | DecCommand FilePath

data PoetryOptions = PoetryOptions
    { formatFilename :: String
    , outputFile :: FilePath
    , outputFormat :: FileFormat
    , commandOpt :: Command }

parseCommand :: Parser Command
parseCommand =
    let parseInput = argument str (metavar "input") in
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
        ( long "output"
       <> short 'o'
       <> metavar "FILENAME"
       <> showDefault
       <> value "-"
       <> help "filename to output to")
    <*> flag AsBase64 AsBinary
        ( long "binary"
       <> short 'b'
       <> help "skip base64 encoding/decoding, treat data as binary")
    <*> parseCommand

parseFile :: FilePath -> IO Document
parseFile fname = do
    d <- withFile fname ReadMode $ \h ->
        parseFormat fname <$> hGetContents h
    case d of
      Left e -> do
        putStrLn "could not parse format"
        print e
        exitFailure
      Right d -> return d

mainOp :: PoetryOptions -> IO ()
mainOp PoetryOptions{formatFilename} = do
    d <- parseFile formatFilename
    print d

main :: IO ()
main = execParser opts >>= mainOp
    where opts = info (parseOpts <**> helper)
            ( fullDesc
           <> progDesc "Encode and decode using a poem format"
           <> header "entropic-poetry: using a poem as an encoding")
