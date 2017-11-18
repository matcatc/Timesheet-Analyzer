-- | This is the "Computation and Analysis" ("comp" for short) component of the Timesheet Analyzer program.
--
-- It is responsible for reading in the user's timesheet CSV file, crunching
-- some numbers, and generating a new CSV file. This new CSV file's format is
-- different from the input file in order to make processing by Gnuplot easier.
--
-- Author: Matthew Todd
-- Date: 2017-09-16

-- For cmdArgs
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Monad
    ( when
    )
import Data.List
    ( intercalate
    )
import System.Directory
    ( doesFileExist
    )

-- TODO: separate out command line arguments stuff to a separate module?
import System.Console.CmdArgs

-- | Default output timesheet
_DEFAULT_OUTPUT_TIMESHEET :: String
_DEFAULT_OUTPUT_TIMESHEET = "intermediate_timesheet.csv"

data Options
    = Options
        { inputTimesheets :: [FilePath]
        , outputTimesheet :: FilePath
        }
    deriving (Data,Show,Typeable)

options :: Options
options = Options
    { inputTimesheets = def &= args &= typ "FILES"
    , outputTimesheet = _DEFAULT_OUTPUT_TIMESHEET &= typ "FILE"
    }

-- |
-- Check that the files exist
checkFilesExist :: [FilePath] -> IO Bool
checkFilesExist files = do
        xs <- mapM checkFile files
        return (and xs)
    where
        checkFile :: FilePath -> IO Bool
        checkFile file = do
                    fileExist <- doesFileExist file
                    if fileExist then
                        return True
                    else
                        putStrLn ("Error: file " ++ file ++ " does not exist.") >> return False

-- |
-- Process the input files
processFiles :: [FilePath] -> IO ()
processFiles files = putStrLn "TODO: implement processFiles"

-- |
-- Main function.
--
main :: IO ()
main = do
    opts <- cmdArgs options
    let inFiles = inputTimesheets opts
        outFile = outputTimesheet opts in 
        if null inFiles then
            putStrLn "Error: no input timesheets provided."
        else do
            filesExist <- checkFilesExist inFiles
            when filesExist $ do
                putStrLn $ "Will process the following timesheets: " ++ intercalate ", " inFiles
                -- TODO: rename function
                processFiles inFiles
                putStrLn $ "Writing to output timesheet: " ++ outFile
                -- TODO: write output file

