-- Liang-Ting Chen 2019-10-13:
-- this program is partially re-written so that the configuration part is
-- controlled by a configuration file `fix-whitespace.yaml" in the base
-- directory instead.

import Control.Monad

import Data.Char as Char
import Data.List.Extra (nubOrd)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text  -- Strict IO.

import System.Directory ( getCurrentDirectory, doesFileExist)
import System.Environment
import System.Exit
import System.FilePath
import System.FilePattern
import System.FilePattern.Directory
import System.IO
import System.Console.GetOpt

import ParseConfig

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

defaultOptions = Options
  { optVerbose = False
  , optHelp    = False
  , optMode    = Fix
  , optConfig  = "fix-whitespace.yaml"
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
      "Override the project configuration fix-whitespace.yaml."
  , Option []        ["check"]
      (NoArg (\opts -> opts { optMode = Check }))
      (unlines
        [ "With --check the program does not change any files,"
        , "it just checks if any files would have been changed."
        , "In this case it returns with a non-zero exit code."
        ])
  ]

compilerOpts :: String -> IO (Options, [String])
compilerOpts progName = do
  argv <- getArgs
  case getOpt Permute options argv of
      (o, n, []  ) -> return (foldl (flip id) defaultOptions o, n)
      (_, _, errs) -> ioError (userError (concat errs ++ "\n" ++ shortUsageHeader progName))

shortUsageHeader, usageHeader, usage :: String -> String

shortUsageHeader progName =
  "Usage: " ++ progName ++ " [-h|--help] [-v|--verbose] [--check] [--config CONFIG] [FILES]"

usageHeader progName = unlines
  [ shortUsageHeader progName
  , ""
  , "The program does the following"
  , ""
  , "* Removes trailing whitespace."
  , "* Removes trailing lines containing nothing but whitespace."
  , "* Ensures that the file ends in a newline character."
  , ""
  , "for files specified in [FILES] or"
  , ""
  , "\t" ++ optConfig defaultOptions
  , ""
  , "under the current directory."
  , ""
  , "Background: Agda was reported to fail to compile on Windows"
  , "because a file did not end with a newline character (Agda"
  , "uses -Werror)."
  , ""
  , "Available options:"
  ]

usage progName = usageInfo (usageHeader progName) options

main :: IO ()
main = do
  progName <- getProgName
  (opts, nonOpts) <- compilerOpts progName

  -- check if the user asks for help
  when (optHelp opts) $ putStr (usage progName) >> exitSuccess

  -- check if the configuration file exists
  configExist <- doesFileExist $ optConfig opts
  unless (configExist || not (null nonOpts)) $ do
    hPutStr stderr (unlines
      [ "fix-whitespace.yaml is not found and there are no files specified as arguments."
      , ""
      , shortUsageHeader progName
      ])
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
fix mode verbose f = do

  new <- withFile f ReadMode $ \h -> do
    hSetEncoding h utf8
    s <- Text.hGetContents h
    let s' = transform s
    return $ if s' == s then Nothing else Just s'
  case new of
    Nothing -> do
      when verbose (putStrLn $ "[ Checked ] " ++ f)
      return False
    Just s  -> do
      hPutStrLn stderr $
        "[ Violation " ++
        (if mode == Fix then "fixed" else "detected") ++
        " ] " ++ f
      when (mode == Fix) $
        withFile f WriteMode $ \h -> do
          hSetEncoding h utf8
          Text.hPutStr h s
      return True

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

  addSpaces 0 x = x
  addSpaces n x = addSpaces (n-1) (' ':x)

-- | 'dropWhile' except keep the first of the dropped elements
dropWhile1 :: (a -> Bool) -> [a] -> [a]
dropWhile1 _ [] = []
dropWhile1 p (x:xs)
  | p x       = x : dropWhile p xs
  | otherwise = x : xs
