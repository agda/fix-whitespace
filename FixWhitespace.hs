-- | Program to enforce a whitespace policy.

import Control.Monad
import Control.Exception (IOException, handle)

import Data.Char as Char
import Data.List.Extra (nubOrd)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text  -- Strict IO.

import System.Directory ( getCurrentDirectory, doesFileExist)
import System.Environment
import System.Exit
-- import System.FilePath
-- import System.FilePattern
import System.FilePattern.Directory
import System.IO
import System.Console.GetOpt

import ParseConfig

-- | Default configuration file.

defaultConfigFile :: String
defaultConfigFile = "fix-whitespace.yaml"

-- Modes.
data Mode
  = Fix    -- ^ Fix whitespace issues.
  | Check  -- ^ Check if there are any whitespace issues.
    deriving (Show, Eq)

type Verbose = Bool

data Options = Options
  { optVerbose :: Verbose
  -- ^ Display the location of a file being checked or not.
  , optHelp    :: Bool
  -- ^ Display the help information.
  , optMode    :: Mode
  , optConfig  :: FilePath
  -- ^ The location to the configuration file.
  }

defaultOptions :: Options
defaultOptions = Options
  { optVerbose = False
  , optHelp    = False
  , optMode    = Fix
  , optConfig  = defaultConfigFile
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['h']     ["help"]
      (NoArg (\opts -> opts { optHelp = True }))
      "Show this help information."
  , Option ['v']     ["verbose"]
      (NoArg (\opts -> opts { optVerbose = True }))
      "Show files as they are being checked."
  , Option []        ["config"]
      (ReqArg (\loc opts -> opts { optConfig = loc }) "CONFIG")
      (concat ["Override the project configuration ", defaultConfigFile, "."])
  , Option []        ["check"]
      (NoArg (\opts -> opts { optMode = Check }))
      (unlines
        [ "With --check the program does not change any files,"
        , "it just checks if any files would have been changed."
        , "In this case it returns with a non-zero exit code."
        ])
  ]

programOpts :: String -> IO (Options, [String])
programOpts progName = do
  argv <- getArgs
  case getOpt Permute options argv of
      (o, n, []  ) -> return (foldl (flip id) defaultOptions o, n)
      (_, _, errs) -> ioError $ userError $ concat errs ++ "\n" ++ shortUsageHeader progName


shortUsageHeader :: String -> String
shortUsageHeader progName =
  "Usage: " ++ progName ++ " [-h|--help] [-v|--verbose] [--check] [--config CONFIG] [FILES]"

usageHeader :: String -> String
usageHeader progName = unlines
  [ shortUsageHeader progName
  , ""
  , "The program does the following"
  , ""
  , "  * Removes trailing whitespace."
  , "  * Removes trailing lines containing nothing but whitespace."
  , "  * Ensures that the file ends in a newline character."
  , "  * Convert tabs to spaces, assuming a tab-size of 8."
  , ""
  , "for files specified in [FILES] or"
  , ""
  , "\t" ++ optConfig defaultOptions
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

  changes <- mapM (fix mode verbose) files

  when (or changes && mode == Check) exitFailure

fix :: Mode -> Verbose -> FilePath -> IO Bool
fix mode verbose f =
  checkFile f >>= \case

    CheckOK -> do
      when verbose $
        putStrLn $ "[ Checked ] " ++ f
      return False

    CheckViolation s  -> do
      hPutStrLn stderr $
        "[ Violation " ++
        (if mode == Fix then "fixed" else "detected") ++
        " ] " ++ f
      when (mode == Fix) $
        withFile f WriteMode $ \h -> do
          hSetEncoding h utf8
          Text.hPutStr h s
      return True

    CheckIOError _e -> do
      hPutStrLn stderr $
        "[ Read error ] " ++ f
      return False

-- | Result of checking a file against the whitespace policy.

data CheckResult
  = CheckOK
      -- ^ The file satifies the policy.
  | CheckViolation Text
      -- ^ The file violates the policy, a fix is returned.
  | CheckIOError IOException
      -- ^ An I/O error occurred while accessing the file.
      --   (E.g., the file is not UTF8 encoded.)

-- | Check a file against the whitespace policy,
--   returning a fix if violations occurred.

checkFile :: FilePath -> IO CheckResult
checkFile f =
  handle (\ (e :: IOException) -> return $ CheckIOError e) $
    withFile f ReadMode $ \ h -> do
      hSetEncoding h utf8
      s <- Text.hGetContents h
      let s' = transform s
      return $ if s' == s then CheckOK else CheckViolation s'

-- | Transforms the contents of a file.

transform :: Text -> Text
transform =
  Text.unlines .
  removeFinalEmptyLinesExceptOne .
  map (removeTrailingWhitespace .  convertTabs) .
  Text.lines
  where
  removeFinalEmptyLinesExceptOne =
    reverse . dropWhile1 Text.null . reverse

  removeTrailingWhitespace =
    Text.dropWhileEnd ((`elem` [Space,Format]) . generalCategory)

  convertTabs =
    Text.pack . reverse . fst . foldl convertOne ([], 0) . Text.unpack

  convertOne (a, p) '\t' = (addSpaces n a, p + n)
                           where
                             n = 8 - p `mod` 8
  convertOne (a, p) c = (c:a, p+1)

  addSpaces :: Int -> String -> String
  addSpaces n = (replicate n ' ' ++)

-- | 'dropWhile' except keep the first of the dropped elements
dropWhile1 :: (a -> Bool) -> [a] -> [a]
dropWhile1 _ [] = []
dropWhile1 p (x:xs)
  | p x       = x : dropWhile p xs
  | otherwise = x : xs
