-- | Program to enforce a whitespace policy.

{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad                ( unless, when )

import           Data.List.Extra              ( nubOrd )
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as Text {- Strict IO -}
import           Data.Version                 ( showVersion )

import           System.Console.GetOpt        ( OptDescr(Option), ArgDescr(NoArg, ReqArg), ArgOrder(Permute), getOpt, usageInfo )
import           System.Directory             ( getCurrentDirectory, doesFileExist )
import           System.Environment           ( getArgs, getProgName )
import           System.Exit                  ( die, exitFailure, exitSuccess )
import           System.FilePattern.Directory ( getDirectoryFiles, getDirectoryFilesIgnore )
import           System.IO                    ( IOMode(WriteMode), hPutStr, hPutStrLn, hSetEncoding, stderr, utf8, withFile )

import           Text.Read                    ( readMaybe )

import           Data.Text.FixWhitespace      ( CheckResult(CheckOK, CheckViolation, CheckIOError), checkFile, displayLineError
                                              , TabSize, Verbose, defaultTabSize )

import           ParseConfig                  ( Config(Config), parseConfig )
import qualified Paths_fix_whitespace         as PFW ( version )

-- | Default configuration file.

defaultConfigFile :: String
defaultConfigFile = "fix-whitespace.yaml"

-- Modes.
data Mode
  = Fix    -- ^ Fix whitespace issues.
  | Check  -- ^ Check if there are any whitespace issues.
    deriving (Show, Eq)

data Options = Options
  { optVerbose :: Verbose
  -- ^ Display the location of a file being checked or not.
  , optHelp    :: Bool
  -- ^ Display the help information.
  , optVersion :: Bool
  -- ^ Display the program's version.
  , optMode    :: Mode
  , optConfig  :: FilePath
  -- ^ The location to the configuration file.
  , optTabSize :: String
  -- ^ The number of spaces to expand a tab character to.  @"0"@ for keeping tabs.
  }

defaultOptions :: Options
defaultOptions = Options
  { optVerbose = False
  , optHelp    = False
  , optVersion = False
  , optMode    = Fix
  , optConfig  = defaultConfigFile
  , optTabSize = show defaultTabSize
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['h']     ["help"]
      (NoArg (\opts -> opts { optHelp = True }))
      "Show this help information."
  , Option ['V']     ["version"]
      (NoArg (\opts -> opts { optVersion = True }))
      "Show the program's version."
  , Option ['v']     ["verbose"]
      (NoArg (\opts -> opts { optVerbose = True }))
      (unlines
        [ "Show files as they are being checked."
        , "Display location of detected whitespace violations."
        ])
  , Option ['t']     ["tab"]
      (ReqArg (\ts opts -> opts { optTabSize = ts }) "TABSIZE")
      (unlines
        [ "Expand tab characters to TABSIZE (default: " ++ show defaultTabSize ++ ") many spaces."
        , "Keep tabs if 0 is given as TABSIZE."
        ])
  , Option []        ["config"]
      (ReqArg (\loc opts -> opts { optConfig = loc }) "CONFIG")
      (concat ["Override the project configuration ", defaultConfigFile, "."])
  , Option []        ["check"]
      (NoArg (\opts -> opts { optMode = Check }))
      (unlines
        [ "With --check the program does not change any files,"
        , "it just checks if any files would have been changed."
        , "In the latter case it returns with a non-zero exit code."
        ])
  ]

programOpts :: String -> IO (Options, [String])
programOpts progName = do
  argv <- getArgs
  case getOpt Permute options argv of
      (o, n, []  ) -> return (foldl (flip id) defaultOptions o, n)
      (_, _, errs) -> ioError $ userError $ concat errs ++ "\n" ++ shortUsageHeader progName


shortUsageHeader :: String -> String
shortUsageHeader progName = unwords
  [ "Usage:"
  , progName
  , "[-h|--help] [-v|--verbose] [--check] [--config CONFIG] [-t|--tab TABSIZE] [FILES]"
  ]

usageHeader :: String -> String
usageHeader progName = unlines
  [ shortUsageHeader progName
  , ""
  , "The program does the following"
  , ""
  , "  * Removes trailing whitespace."
  , "  * Removes trailing lines containing nothing but whitespace."
  , "  * Ensures that the file ends in a newline character."
  , "  * Convert tabs to TABSIZE (default: " ++ show defaultTabSize ++ ") spaces, unless TABSIZE is set to 0."
  , ""
  , "for files specified in [FILES] or"
  , ""
  , "\t" ++ defaultConfigFile
  , ""
  , "under the current directory."
  , ""
  , "Available options:"
  ]

usage :: String -> String
usage progName = usageInfo (usageHeader progName) options

main :: IO ()
main = do
  progName <- getProgName
  (opts, nonOpts) <- programOpts progName

  -- check if the user asks for help
  when (optHelp opts) $ putStr (usage progName) >> exitSuccess

  -- check if the user asks for the program's version
  when (optVersion opts) $ putStrLn (showVersion PFW.version) >> exitSuccess

  -- check if the configuration file exists
  configExist <- doesFileExist $ optConfig opts
  unless (configExist || not (null nonOpts)) $ do
    hPutStr stderr $ unlines
      [ unwords [defaultConfigFile, "is not found and there are no files specified as arguments."]
      , ""
      , shortUsageHeader progName
      ]
    exitFailure

  let mode    = optMode    opts
      verbose = optVerbose opts
      config  = optConfig  opts

  tabSize <- maybe (die "Error: Illegal TABSIZE, must be an integer.") return $
    readMaybe $ optTabSize opts

  base <- getCurrentDirectory

  files <- if not $ null nonOpts
    then getDirectoryFiles base nonOpts
    else do
      Config incDirs0 excDirs0 incFiles excFiles <- parseConfig config
      let incDirs = map (++ "/**/") incDirs0
      let excDirs = map (++ "/**/") excDirs0

      -- File patterns to always include
      -- when not matching an excluded file pattern
      let incWhitelistPatterns = concatMap (\d -> map (d ++) incFiles) incDirs
      -- File patterns to always exclude
      let excBlacklistPatterns = map ("**/" ++) excFiles

      -- Files to include when not in an excluded directory
      -- and when not matching an excluded file pattern
      let incPatterns = map ("**/" ++) incFiles
      -- Directory and file patterns to exclude
      let excPatterns = (map (++ "*") excDirs)
                     ++ (map ("**/" ++) excFiles)

      when verbose $ do
        putStrLn "Include whitelist:"
        putStrLn (unlines incWhitelistPatterns)

        putStrLn "Exclude blacklist:"
        putStrLn (unlines excBlacklistPatterns)

        putStrLn "Include:"
        putStrLn (unlines incPatterns)

        putStrLn "Exclude:"
        putStrLn (unlines excPatterns)

      files0 <- getDirectoryFilesIgnore base incWhitelistPatterns excBlacklistPatterns
      files1 <- getDirectoryFilesIgnore base incPatterns excPatterns
      return (nubOrd (files0 ++ files1))

  changes <- mapM (fix mode verbose tabSize) files

  when (or changes && mode == Check) exitFailure

fix :: Mode -> Verbose -> TabSize -> FilePath -> IO Bool
fix mode verbose tabSize f =
  checkFile tabSize verbose f >>= \case

    CheckOK -> do
      when verbose $
        putStrLn $ "[ Checked ] " ++ f
      return False

    CheckViolation s vs ->  do
      hPutStrLn stderr (msg vs)
      when (mode == Fix) $
        withFile f WriteMode $ \h -> do
          hSetEncoding h utf8
          Text.hPutStr h s
      return True

    CheckIOError _e -> do
      hPutStrLn stderr $
        "[ Read error ] " ++ f
      return False

  where
    msg vs
      | mode == Fix =
        "[ Violation fixed ] " ++ f

      | otherwise =
        "[ Violation detected ] " ++ f ++
        (if not verbose then "" else
           ":\n" ++ unlines (map (Text.unpack . displayLineError f) vs))
