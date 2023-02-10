-- | Program to enforce a whitespace policy.

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.Trans.Writer.Strict
import Control.Exception (IOException, handle)

import Data.Char as Char
import Data.List.Extra (nubOrd)
import Data.Text (Text)
import Data.Version (showVersion)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text  -- Strict IO.

import System.Directory (getCurrentDirectory, doesFileExist)
import System.Environment
import System.Exit
-- import System.FilePath
-- import System.FilePattern
import System.FilePattern.Directory (getDirectoryFiles, getDirectoryFilesIgnore)
import System.IO
import System.Console.GetOpt

import Text.Read (readMaybe)

import ParseConfig
import qualified Paths_fix_whitespace as PFW (version)

-- | Default configuration file.

defaultConfigFile :: String
defaultConfigFile = "fix-whitespace.yaml"

-- | Default tab size.

defaultTabSize :: String
defaultTabSize = "8"

-- Modes.
data Mode
  = Fix    -- ^ Fix whitespace issues.
  | Check  -- ^ Check if there are any whitespace issues.
    deriving (Show, Eq)

type Verbose = Bool
type TabSize = Int

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
  , optTabSize = defaultTabSize
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['h']     ["help"]
      (NoArg (\opts -> opts { optHelp = True }))
      "Show this help information."
  , Option []        ["version"]
      (NoArg (\opts -> opts { optVersion = True }))
      "Show the program's version."
  , Option ['v']     ["verbose"]
      (NoArg (\opts -> opts { optVerbose = True }))
      "Show files as they are being checked."
  , Option ['t']     ["tab"]
      (ReqArg (\ts opts -> opts { optTabSize = ts }) "TABSIZE")
      (unlines
        [ "Expand tab characters to TABSIZE (default: " ++ defaultTabSize ++ ") many spaces."
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
  , "  * Convert tabs to TABSIZE (default: " ++ defaultTabSize ++ ") spaces, unless TABSIZE is set to 0."
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
  checkFile tabSize f >>= \case

    CheckOK -> do
      when verbose $
        putStrLn $ "[ Checked ] " ++ f
      return False

    CheckViolation s vs -> do
      hPutStrLn stderr $
        "[ Violation " ++
        (if mode == Fix then "fixed" else "detected") ++
        " ] " ++ f ++ ":\n" ++ (unlines $ map displayViolations vs)

      when (mode == Fix) $
        withFile f WriteMode $ \h -> do
          hSetEncoding h utf8
          Text.hPutStr h s
      return True

    CheckIOError _e -> do
      hPutStrLn stderr $
        "[ Read error ] " ++ f
      return False

-- | Represents a line of input violating whitespace rules.
--   Stores the index of the line and the line itself.
data LineViolating = LV Int Text

displayViolations :: LineViolating -> String
displayViolations (LV i l) = "line " ++ show i ++ "] " ++
  (Text.unpack $ visibleSpaces l)

-- | The transformation monad: maintains info about lines that
--   violate the rules.
type TransformM = Writer [LineViolating]

-- | Result of checking a file against the whitespace policy.

data CheckResult
  = CheckOK
      -- ^ The file satifies the policy.
  | CheckViolation Text [LineViolating]
      -- ^ The file violates the policy, a fix and a list of
      --   violating lines are returned.
  | CheckIOError IOException
      -- ^ An I/O error occurred while accessing the file.
      --   (E.g., the file is not UTF8 encoded.)

-- | Check a file against the whitespace policy,
--   returning a fix if violations occurred.

checkFile :: TabSize -> FilePath -> IO CheckResult
checkFile tabSize f =
  handle (\ (e :: IOException) -> return $ CheckIOError e) $
    withFile f ReadMode $ \ h -> do
      hSetEncoding h utf8
      s <- Text.hGetContents h
      let (s', lvs) = transform tabSize s
      return $ if s' == s then CheckOK else CheckViolation s' lvs

-- | Transforms the contents of a file.

transform
  :: TabSize   -- ^ Expand tab characters to so many spaces.  Keep tabs if @<= 0@.
  -> Text      -- ^ Text before transformation.
  -> (Text, [LineViolating]) -- ^ Text after transformation and violating lines if any.
transform tabSize =
  runWriter .
  fmap Text.unlines .
  fixAllViolations .
  zip [1..] .
  Text.lines
  where
  fixAllViolations :: [(Int,Text)] -> TransformM [Text]
  fixAllViolations = removeFinalEmptyLinesExceptOne <=<
    mapM (fixLineWith $ removeTrailingWhitespace . convertTabs)

  removeFinalEmptyLinesExceptOne :: [Text] -> TransformM [Text]
  removeFinalEmptyLinesExceptOne ls
    | lenLs == lenLs'= pure ls
    | otherwise = tell (map (uncurry LV) $ zip [1+lenLs' ..] els) >> pure ls'
    where
    ls' = reverse . dropWhile1 Text.null . reverse $ ls
    lenLs = length ls
    lenLs' = length ls'
    els = replicate (lenLs - lenLs') ""

  removeTrailingWhitespace =
    Text.dropWhileEnd $ \ c -> generalCategory c `elem` [Space,Format] || c == '\t'

  fixLineWith :: (Text -> Text) -> (Int, Text) -> TransformM Text
  fixLineWith fixer (i, l)
    | l == l' = pure l
    | otherwise = tell [LV i l] >> pure l'
    where l' = fixer l


  convertTabs = if tabSize <= 0 then id else
    Text.pack . reverse . fst . foldl convertOne ([], 0) . Text.unpack

  convertOne (a, p) '\t' = (addSpaces n a, p + n)
                           where
                             n = tabSize - p `mod` tabSize  -- Here, tabSize > 0 is guaranteed
  convertOne (a, p) c = (c:a, p+1)

  addSpaces :: Int -> String -> String
  addSpaces n = (replicate n ' ' ++)

-- | 'dropWhile' except keep the first of the dropped elements
dropWhile1 :: (a -> Bool) -> [a] -> [a]
dropWhile1 _ [] = []
dropWhile1 p (x:xs)
  | p x       = x : dropWhile p xs
  | otherwise = x : xs

-- | Replace spaces and tabs with visible characters for
--   presentation purposes. Space turns into '·' and tab into '→'
visibleSpaces :: Text -> Text
visibleSpaces s
  | Text.null s = "<empty line>"
  | otherwise = Text.replace "\t" "→" . Text.replace " " "·" $ s
