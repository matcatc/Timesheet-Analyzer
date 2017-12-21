-- | This is the "Computation and Analysis" ("comp" for short) component of the Timesheet Analyzer program.
--
-- It is responsible for reading in the user's timesheet CSV file, crunching
-- some numbers, and generating a new CSV file. This new CSV file's format is
-- different from the input file in order to make processing by Gnuplot easier.
--
-- Author: Matthew Todd
-- Date: 2017-09-16

-- TODO: switch from String to ByteString or Text?
-- TODO: switch to a streaming based setup (instead of just lazy datatypes)?
--          Should only be necessary when processing really large inputs, so handle later.
-- TODO: switch out for a different CSV library later?

-- For cmdArgs
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Monad
    ( when
    )
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Char
    ( isSpace
    )
import Data.Csv
import Data.Either
    ( partitionEithers
    )
import Data.List
    ( foldl'
    , groupBy
    , intercalate
    , sortBy
    )
import Data.Vector
    ( Vector
    , toList
    , fromList
    )
import GHC.Generics
    ( Generic
    )
import System.Directory
    ( doesFileExist
    )

-- TODO: separate out command line arguments stuff to a separate module?
import System.Console.CmdArgs

-- | Default output timesheet
_DEFAULT_OUTPUT_TIMESHEET :: String
_DEFAULT_OUTPUT_TIMESHEET = "intermediate_timesheet.csv"

-- | Header information for output timesheet
_OUTPUT_TIMESHEET_HEADER :: String
_OUTPUT_TIMESHEET_HEADER = "# Generated by Timesheet Analyzer"

-- Category names
_CATEGORY_LABOR :: String
_CATEGORY_LABOR = "labor"

_CATEGORY_SICK :: String
_CATEGORY_SICK = "sick"

_CATEGORY_VACATION :: String
_CATEGORY_VACATION = "vacation"

_CATEGORY_HOLIDAY :: String
_CATEGORY_HOLIDAY = "holiday"

_CATEGORY_COMP :: String
_CATEGORY_COMP = "comp"

-- | Comment character in timesheet CSV files
_COMMENT_CHARACTER :: Char
_COMMENT_CHARACTER = '#'

-- | Number of hours in a normal work week
-- TODO: make configurable?
_NORMAL_WORK_WEEK_HOURS :: Float
_NORMAL_WORK_WEEK_HOURS = 40

-- |
-- Numbers for a particular week
data WeekNumbers
    = WeekNumbers
        { monday :: Float
        , tuesday :: Float
        , wednesday :: Float
        , thursday :: Float
        , friday :: Float
        , saturday :: Float
        , sunday :: Float
        , total :: Float
        }
    -- TODO: derive other stuff?
    deriving (Show)

-- | empty WeekNumbers. Useful starting point.
emptyWeekNumbers :: WeekNumbers
emptyWeekNumbers = WeekNumbers
    { monday = 0
    , tuesday = 0
    , wednesday = 0
    , thursday = 0
    , friday = 0
    , saturday = 0
    , sunday = 0
    , total = 0
    }

-- | Determine whether a week has any non-zero hours in it
isEmptyWeek :: WeekNumbers
            -> Bool     -- ^ True if empty. False if at least one day is non-zero.
isEmptyWeek (WeekNumbers 0 0 0 0 0 0 0 0) = True
isEmptyWeek _ = False

-- | Add two week numbers structs together
addWeekNumbers :: WeekNumbers -> WeekNumbers -> WeekNumbers
addWeekNumbers l r = WeekNumbers
    { monday = (monday l) + (monday r)
    , tuesday = (tuesday l) + (tuesday r)
    , wednesday = (wednesday l) + (wednesday r)
    , thursday = (thursday l) + (thursday r)
    , friday = (friday l) + (friday r)
    , saturday = (saturday l) + (saturday r)
    , sunday = (sunday l) + (sunday r)
    , total = (total l) + (total r)
    }

-- |
-- Our internal data type representation of the timesheet data for a week
-- TODO: rename?
data DataStruct
    = DataStruct
        { weekId :: String
        , laborHours :: WeekNumbers
        , sickHours :: WeekNumbers
        , vacationHours :: WeekNumbers
        , holidayHours :: WeekNumbers
        , compHours :: WeekNumbers
        , totalHours :: WeekNumbers -- ^ total hours per day
        , compEarned :: Float -- ^ how much comp-time was earned this week. Can be negative.
        }
    -- TODO: add total hours?
    -- TODO: derive other stuff?
    deriving (Show)

-- | Empty DataStruct. Useful for starting point
emptyDataStruct :: DataStruct
emptyDataStruct = DataStruct
    { weekId = ""
    , laborHours = emptyWeekNumbers
    , sickHours = emptyWeekNumbers
    , vacationHours = emptyWeekNumbers
    , holidayHours = emptyWeekNumbers
    , compHours = emptyWeekNumbers
    , totalHours = emptyWeekNumbers
    , compEarned = 0.0
    }

-- | Determine whether a data struct has any non-zero hours in it
isEmptyDataStruct :: DataStruct
                  -> Bool     -- ^ True if empty. False if at least one day is non-zero.
isEmptyDataStruct x = laborEmpty && sickEmpty && vacationEmpty && holidayEmpty && compEmpty && totalEmpty && compEarnedEmpty
    where
        laborEmpty = isEmptyWeek $ laborHours x
        sickEmpty = isEmptyWeek $ sickHours x
        vacationEmpty = isEmptyWeek $ vacationHours x
        holidayEmpty = isEmptyWeek $ holidayHours x
        compEmpty = isEmptyWeek $ compHours x
        totalEmpty = isEmptyWeek $ totalHours x
        compEarnedEmpty = 0 == compEarned x
        

-- | Data structs are for the same week
isSameWeek :: DataStruct -> DataStruct -> Bool
isSameWeek l r = weekId l == weekId r

-- | Add two data structs together
-- All data structs must be for the same week, or this function will fail.
-- TODO: proper error handling
addDataStructs :: DataStruct -> DataStruct -> DataStruct
addDataStructs l r =
    if not (isSameWeek l r) then
        error "Both data structs are not for the same week!"
    else
        DataStruct
        { weekId = weekId l
        , laborHours = addWeekNumbers (laborHours l) (laborHours r)
        , sickHours = addWeekNumbers (sickHours l) (sickHours r)
        , vacationHours = addWeekNumbers (vacationHours l) (vacationHours r)
        , holidayHours = addWeekNumbers (holidayHours l) (holidayHours r)
        , compHours = addWeekNumbers (compHours l) (compHours r)
        , totalHours = addWeekNumbers (totalHours l) (totalHours r)
        , compEarned = (compEarned l) + (compEarned r)
        }

-- | Merge multiple data structs for the same week.
-- All data structs must be for the same week, or this function will fail.
-- TODO: proper error handling
mergeDataStructs :: [DataStruct] -- ^ Multiple data structs for the same week
                 -> DataStruct   -- ^ Single data struct for the same week
mergeDataStructs xs = foldl' addDataStructs starterDataStruct xs
    where
        -- | an empty data struct, but with the week ID properly filled in for the fold
        starterDataStruct = emptyDataStruct { weekId = weekId (head xs) }

-- | Group DataStructs that are for the same week
groupByWeek :: [DataStruct]   -- ^ List of data structs for multiple weeks
            -> [[DataStruct]] -- ^ List of Lists of data structs for each week
groupByWeek = groupBy isSameWeek

-- | Sort a list of DataStructs by week id
sortDataStructs :: [DataStruct] -> [DataStruct]
sortDataStructs = sortBy comp
    where
        comp :: DataStruct -> DataStruct -> Ordering
        comp l r = compare (weekId l) (weekId r)

-- TODO: Instead of going through tuples, encode/decode directly from/to DataStruct.
-- See http://hackage.haskell.org/package/cassava-0.5.1.0/docs/Data-Csv.html

-- | Type representing a single line of CSV from an input file
type InputCsvLine =
    ( String -- ^ Week ID
    , String -- ^ category
    , Float  -- ^ Monday hours
    , Float  -- ^ Tuesday hours
    , Float  -- ^ Wednesday hours
    , Float  -- ^ Thursday hours
    , Float  -- ^ Friday hours
    , Float  -- ^ Saturday hours
    , Float  -- ^ Sunday hours
    )

-- | Type representing a single line of CSV for the output file
type OutputCsvLine =
    ( String -- ^ Week ID
    , Float  -- ^ Monday labor hours
    , Float  -- ^ Monday sick hours
    , Float  -- ^ Monday vacation hours
    , Float  -- ^ Monday holiday hours
    , Float  -- ^ Monday comp hours
    , Float  -- ^ Monday total hours
    , Float  -- ^ Tuesday labor hours
    , Float  -- ^ Tuesday sick hours
    , Float  -- ^ Tuesday vacation hours
    , Float  -- ^ Tuesday holiday hours
    , Float  -- ^ Tuesday comp hours
    , Float  -- ^ Tuesday total hours
    , Float  -- ^ Wednesday labor hours
    , Float  -- ^ Wednesday sick hours
    , Float  -- ^ Wednesday vacation hours
    , Float  -- ^ Wednesday holiday hours
    , Float  -- ^ Wednesday comp hours
    , Float  -- ^ Wednesday total hours
    , Float  -- ^ Thursday labor hours
    , Float  -- ^ Thursday sick hours
    , Float  -- ^ Thursday vacation hours
    , Float  -- ^ Thursday holiday hours
    , Float  -- ^ Thursday comp hours
    , Float  -- ^ Thursday total hours
    , Float  -- ^ Friday labor hours
    , Float  -- ^ Friday sick hours
    , Float  -- ^ Friday vacation hours
    , Float  -- ^ Friday holiday hours
    , Float  -- ^ Friday comp hours
    , Float  -- ^ Friday total hours
    , Float  -- ^ Saturday labor hours
    , Float  -- ^ Saturday sick hours
    , Float  -- ^ Saturday vacation hours
    , Float  -- ^ Saturday holiday hours
    , Float  -- ^ Saturday comp hours
    , Float  -- ^ Saturday total hours
    , Float  -- ^ Sunday labor hours
    , Float  -- ^ Sunday sick hours
    , Float  -- ^ Sunday vacation hours
    , Float  -- ^ Sunday holiday hours
    , Float  -- ^ Sunday comp hours
    , Float  -- ^ Sunday total hours
    , Float  -- ^ total labor hours
    , Float  -- ^ total sick hours
    , Float  -- ^ total vacation hours
    , Float  -- ^ total holiday hours
    , Float  -- ^ total comp hours
    , Float  -- ^ week total hours
    , Float  -- ^ comp earned this week
    , Float  -- ^ comp accrued/accumulated after this week
    )

-- TODO: delete when done testing (this is for ghci convenience)
testOutputCsvLine =
    ( "testId"
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    )

-- | Convert an OutputCsvnLine to a CSV record for use by Data.Csv
-- Interestingly an equivalent isn't needed for InputCsvLine, but we need this
-- for OutputCsvLine. This is likely due to the lenght of the OutputCsvLine
-- tuple.
instance (ToField f1, ToField f2, ToField f3, ToField f4, ToField f5, ToField f6,
          ToField f7, ToField f8, ToField f9, ToField f10, ToField f11, ToField f12,
          ToField f13, ToField f14, ToField f15, ToField f16, ToField f17,
          ToField f18, ToField f19, ToField f20, ToField f21, ToField f22,
          ToField f23, ToField f24, ToField f25, ToField f26, ToField f27,
          ToField f28, ToField f29, ToField f30, ToField f31, ToField f32,
          ToField f33, ToField f34, ToField f35, ToField f36, ToField f37,
          ToField f38, ToField f39, ToField f40, ToField f41, ToField f42,
          ToField f43, ToField f44, ToField f45, ToField f46, ToField f47,
          ToField f48, ToField f49, ToField f50, ToField f51) =>
         ToRecord (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14,
                   f15, f16, f17, f18, f19, f20, f21, f22, f23, f24, f25, f26,
                   f27, f28, f29, f30, f31, f32, f33, f34, f35, f36, f37, f38,
                   f39, f40, f41, f42, f43, f44, f45, f46, f47, f48, f49, f50, f51) where
    toRecord (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15,
              f16, f17, f18, f19, f20, f21, f22, f23, f24, f25, f26, f27, f28,
              f29, f30, f31, f32, f33, f34, f35, f36, f37, f38, f39, f40, f41,
              f42, f43, f44, f45, f46, f47, f48, f49, f50, f51) = fromList [
        toField f1, toField f2, toField f3, toField f4, toField f5, toField f6,
        toField f7, toField f8, toField f9, toField f10, toField f11, toField f12,
        toField f13, toField f14, toField f15, toField f16, toField f17,
        toField f19, toField f19, toField f20, toField f21, toField f22,
        toField f23, toField f24, toField f25, toField f26, toField f27,
        toField f29, toField f29, toField f30, toField f31, toField f32,
        toField f33, toField f34, toField f35, toField f36, toField f37,
        toField f39, toField f39, toField f40, toField f41, toField f42,
        toField f43, toField f44, toField f45, toField f46, toField f47,
        toField f48, toField f49, toField f50, toField f51]


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
checkFilesExist :: [FilePath] -- ^ Files to veify exist
                -> IO Bool    -- ^ True if all files exist, False otherwise
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
-- Check all of the user provided input.
-- Prints error messages to console.
checkUserInput :: [FilePath] -- ^ List of input files
               -> FilePath   -- ^ Output file
               -> IO Bool    -- ^ True if everything looks good. False otherwise.
-- TODO: check outFile?
checkUserInput inFiles _ = 
        if null inFiles then
            putStrLn "Error: no input timesheets provided. See `--help`" >> return False
        else 
            checkFilesExist inFiles

-- | 
-- Convert a single CSV line to our internal data type representation
convertInputCsvLine :: InputCsvLine -- ^ Our CSV line
               -> Either String DataStruct    -- ^ Either an error message or the converted data
convertInputCsvLine (week, category, mon, tue, wed, thu, fri, sat, sun) = 
        case stripWhitespace category of
            -- TODO: can't use the constants here b/c case creates new variables during matching. Find another way that allows us to use the constants.
            "labor" -> Right $ weekDataStruct {laborHours = weekHours}
            "sick" -> Right $ weekDataStruct {sickHours = weekHours}
            "vacation" -> Right $ weekDataStruct {vacationHours = weekHours}
            "holiday" -> Right $ weekDataStruct {holidayHours = weekHours}
            "comp" -> Right $ weekDataStruct {compHours = weekHours}
            _ -> Left $ "Warning: unknown charge category '" ++ category ++ "'" 
    where
        weekDataStruct :: DataStruct
        weekDataStruct = emptyDataStruct {weekId = stripWhitespace week}

        -- compute total later in the program
        weekHours :: WeekNumbers
        weekHours = WeekNumbers mon tue wed thu fri sat sun 0

        -- TODO: raise to be top-level?
        stripWhitespace :: String -> String
        stripWhitespace = filter (not . isSpace)

-- | Strip lines starting with comments
stripCommentLines :: BS.ByteString -> BS.ByteString
stripCommentLines xs = bsUnlines $ filter (not . startsWithComment) $ bsLines xs
    where
        bsLines :: BS.ByteString -> [BS.ByteString]
        bsLines = C.split '\n'

        bsUnlines :: [BS.ByteString] -> BS.ByteString
        bsUnlines = C.intercalate (C.singleton '\n')

        startsWithComment :: BS.ByteString -> Bool
        startsWithComment = BS.isPrefixOf (C.singleton _COMMENT_CHARACTER)

-- | Get cleaned input file contents
getCleanFileContents :: [FilePath] -> IO BS.ByteString
getCleanFileContents files = do
                fileContents <- mapM BS.readFile files
                return $ stripCommentLines $ BS.concat fileContents

-- | Convert a list of input CSV lines to a list of our internal data type
-- Filters out empty days (days with 0 hours)
-- TODO: name
convertInputCsvLines :: [InputCsvLine] -> Either [String] [DataStruct]
convertInputCsvLines inputCsvLines = case partitionEithers (map convertInputCsvLine inputCsvLines) of
    ([], xs)    -> Right $ filter (not . isEmptyDataStruct) xs
    (errors, _) -> Left errors

-- | Convert a single DataStruct to a single output line for printing to the output CSV file
-- TODO: finish implementing
convertOutputCsvLine :: DataStruct -> OutputCsvLine
convertOutputCsvLine x =
    ( weekId x
    , monday $ laborHours x
    , monday $ sickHours x
    , monday $ vacationHours x
    , monday $ holidayHours x
    , monday $ compHours x
    , monday $ totalHours x
    , tuesday $ laborHours x
    , tuesday $ sickHours x
    , tuesday $ vacationHours x
    , tuesday $ holidayHours x
    , tuesday $ compHours x
    , tuesday $ totalHours x
    , wednesday $ laborHours x
    , wednesday $ sickHours x
    , wednesday $ vacationHours x
    , wednesday $ holidayHours x
    , wednesday $ compHours x
    , wednesday $ totalHours x
    , thursday $ laborHours x
    , thursday $ sickHours x
    , thursday $ vacationHours x
    , thursday $ holidayHours x
    , thursday $ compHours x
    , thursday $ totalHours x
    , friday $ laborHours x
    , friday $ sickHours x
    , friday $ vacationHours x
    , friday $ holidayHours x
    , friday $ compHours x
    , friday $ totalHours x
    , saturday $ laborHours x
    , saturday $ sickHours x
    , saturday $ vacationHours x
    , saturday $ holidayHours x
    , saturday $ compHours x
    , saturday $ totalHours x
    , sunday $ laborHours x
    , sunday $ sickHours x
    , sunday $ vacationHours x
    , sunday $ holidayHours x
    , sunday $ compHours x
    , sunday $ totalHours x
    , total $ laborHours x
    , total $ sickHours x
    , total $ vacationHours x
    , total $ holidayHours x
    , total $ compHours x
    , total $ totalHours x
    , compEarned x
    , 0  -- TODO: accrued/accumulated after this week
    )

-- | Convert a list of DataStructs to a list of CSV lines
convertOutputCsvLines :: [DataStruct] -> [OutputCsvLine]
convertOutputCsvLines = map convertOutputCsvLine

-- | Compute the total / sum hours for a week
computeWeeklyTotal :: WeekNumbers -> WeekNumbers
computeWeeklyTotal x = WeekNumbers
    { monday = monday x
    , tuesday = tuesday x
    , wednesday = wednesday x
    , thursday = thursday x
    , friday = friday x
    , saturday = saturday x
    , sunday = sunday x
    , total = (monday x)
            + (tuesday x)
            + (wednesday x)
            + (thursday x)
            + (friday x)
            + (saturday x)
            + (sunday x)
    }

-- | Compute the various metrics for the week
computeWeeklyMetrics :: DataStruct -> DataStruct
-- TODO: implement. Dummied to return without computing anything for now
--computeWeeklyMetrics = id
computeWeeklyMetrics x = DataStruct
        { weekId = weekId x
        , laborHours = labor
        , sickHours = sick
        , vacationHours = vacation
        , holidayHours = holiday
        , compHours = comp
        , totalHours = totalHours
        , compEarned = compEarned
        }
    where 
        labor = computeWeeklyTotal $ laborHours x
        sick = computeWeeklyTotal $ sickHours x
        vacation = computeWeeklyTotal $ vacationHours x
        holiday = computeWeeklyTotal $ holidayHours x
        comp = computeWeeklyTotal $ compHours x

        categoryList = [labor, sick, vacation, holiday, comp]
        preTotal = WeekNumbers
            { monday = sum $ map monday categoryList
            , tuesday = sum $ map tuesday categoryList
            , wednesday = sum $ map wednesday categoryList
            , thursday = sum $ map thursday categoryList
            , friday = sum $ map friday categoryList
            , saturday = sum $ map saturday categoryList
            , sunday = sum $ map sunday categoryList
            -- Sum total below
            , total = 0
            }
        totalHours = computeWeeklyTotal preTotal 

        -- comp time earned is time worked over the normal 40 hour week, minus any comp time taken.
        compEarned = (total totalHours) - _NORMAL_WORK_WEEK_HOURS - (total comp)

-- |
-- Process the input files
-- TODO: use Either to return success status?
-- TODO: rename function
-- TODO: clean up function (refactor, split, etc.) Goal is to make it easier to use in ghci
processInput :: BS.ByteString -- ^ Input timesheet data
             -> BS.ByteString -- ^ Output timesheet data to be written
processInput fileContents =
                -- Convert text into CSV and check for any errors
                let eith = decode NoHeader fileContents :: Either String (Vector InputCsvLine) in
                    -- Check for any warnings / errors from decoding
                    case eith of
                        -- TODO: return error (Either?)
                        Left errorStr -> error $ "processing failed: " ++ errorStr
                        -- TODO: take input data structs, merge them per week and compute weekly metrics
                        Right csvLines -> let eith2 = convertInputCsvLines (toList csvLines) in
                            case eith2 of
                                -- TODO: return error (Either?)
                                Left errorStr -> error "processing failed " -- : errorStr
                                Right xs -> let weeklyDataStructs = map mergeDataStructs (groupByWeek xs)
                                                weeklyMetrics = map computeWeeklyMetrics weeklyDataStructs
                                                sortedMetrics = sortDataStructs weeklyMetrics
                                                outCsvLines = convertOutputCsvLines sortedMetrics in
                                                    -- TODO: add CSV header on the first line
                                                    encode outCsvLines


-- |
-- Main function.
main :: IO ()
main = do
    opts <- cmdArgs options
    let inFiles = inputTimesheets opts
        outFile = outputTimesheet opts in do
            userInputOk <- checkUserInput inFiles outFile
            when userInputOk $ do
                putStrLn $ "Will process the following timesheets: " ++ intercalate ", " inFiles
                fileContents <- getCleanFileContents inFiles
                let outputMetrics = processInput fileContents in do
                    -- Build outputMetrics before we print that we're writing to the file (TODO: might need to make other changes)
                    putStrLn $ seq outputMetrics ("Writing to output timesheet: " ++ outFile)
                    BS.writeFile outFile outputMetrics

