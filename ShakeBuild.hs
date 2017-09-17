-- Build rules for Timesheet Analyzer project
--
-- This build system will build the application twice, once for the main
-- binary, once for the unit-test binary. As such, we have two build
-- directories: _build and _build/test. This makes it easier to figure out what
-- files are what, and also avoid unnecessary rebuilding (due to ghc
-- overwriting files for the two binaries).
--
-- Please note that Shake doesn't handle specifying commands to create the
-- target _build and _build/test directories very well. While initially
-- developing this build script, I added targets for _build and _build/test,
-- but then later this broke the build. According to
-- https://hackage.haskell.org/package/shake-0.15.10/docs/Development-Shake.html#v:need
-- and http://shakebuild.com/faq#how-can-i-depend-on-directories, using need on
-- a directory will not work as desired. Furthermore, targets are supposed to
-- create directories automatically if they're needed.
--
-- @author: Matthew Todd
-- @date: 2016-06-25

-- TODO: refactor

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util


main :: IO ()
-- Use ChangeModtime so that a touch or any change to a file should cause it to
-- be rebuilt. This should be the default, according to the Shake
-- documentation, but updating some files wasn't being rebuilt until I manually
-- specified it.
main = shakeArgs shakeOptions{shakeFiles="_build", shakeChange=ChangeModtime} $ do
         -- Main executable
    want [ "_build/TimesheetAnalyzerComp" <.> exe
         -- TODO: bash auto-completion?
         --, "_build/boring_todo.comp"
         -- Test executable and results
         -- TODO: tests?
         --, "_build/test/boring_todo_test" <.> exe
         --, "_build/test/boring_todo_test.tix"
         --, "_build/test/hpc_index.html"
         -- Docs
         , "_build/README.html"
         -- Haddock
         --   Other haddock files will be built at same time, so wanting the main file is enough
         --   TODO: haddock?
         --, "_build/haddock/index.html"
         -- Example / test output from gnuplot
         --  This makes it easier to modify the gnuplot file
         , "_build/gnuplot_test/total_week_time_linechart.png"
         , "_build/gnuplot_test/total_week_time_stacked_linechart.png"
         ]

    phony "clean" $ do
        putNormal "Cleaning files in _build"
        removeFilesAfter "_build" ["//*"]

    -- Main binary
    "_build/TimesheetAnalyzerComp" <.> exe %> \out -> do
        -- hss = Haskell .hs files
        hss <- getDirectoryFiles "" ["//*.hs"]
        -- ins = input .hs files
        --   Ignore the Shake build file
        let ins = filter (/= "ShakeBuild.hs") hss
        need ins
        -- use *dir options to drop all off the created files in _build, so
        -- that they don't pollute the source tree and so we can easily clean
        -- them later
        cmd "ghc --make -hidir ./_build -odir ./_build -Wall -o " [out] ins

    -- Build the user/developer html files from asciidoc files
    "_build/*.html" %> \out -> do
        -- ascii = Asciidoc file
        let ascii = dropDirectory1 $ out -<.> "asciidoc"
        need [ascii]
        cmd "asciidoctor-diagram --out-file" [out] [ascii]

    -- Build the test gnuplot graphs
    "_build/gnuplot_test/*.png" %>
        cmd "gnuplot -c graph.gnuplot ./test_intermediate_timesheet.csv ./_build/gnuplot_test/"

