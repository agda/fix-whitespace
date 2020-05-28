-- Liang-Ting Chen 2019-10-13:
-- this program is partially re-written such that
-- the configuration part is controllfed by an external
-- configuration file `fix-whitespace.yaml"
-- in the base directory instead.

import Control.Monad

import Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text  -- Strict IO.

import System.Directory ( getCurrentDirectory, doesFileExist)
import System.Environment
import System.Exit
import System.FilePath
import System.FilePath.Find
import System.FilePath.Glob
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

  files <- if not $ null nonOpts
    then concat <$> traverse namesMatching nonOpts
    else do
      Config incDirs excDirs incFiles excFiles <- parseConfig config
      base <- getCurrentDirectory
      find (validDir base incDirs excDirs) (validFile base incFiles excFiles) base

  changes <- mapM (fix mode verbose) files

  when (or changes && mode == Check) exitFailure

-- Directory filter
validDir
  :: FilePath
     -- ^ The base directory.
  -> [FilePath]
     -- ^ The list of included directories.
  -> [FilePath]
     -- ^ The list of excluded directories if *not* included above.
  -> RecursionPredicate
validDir base incDirs excDirs =
      foldr (||?) never  (test (~~?) <$> incDirs)
  ||? foldr (&&?) always (test (/~?) <$> excDirs)
  where
  test op = op (fixPath <$> filePath) . (fixPath (addTrailingPathSeparator base) ++)

-- File filter
validFile
  :: FilePath
     -- ^ The base directory.
  -> [FilePath]
     -- ^ The list of files to check.
  -> [FilePath]
     -- ^  The list of excluded file names if included above.
  -> FindClause Bool
validFile base incFiles excFiles =
      foldr (||?) never  (test (~~?) <$> incFiles)
  &&? foldr (&&?) always (test (/~?) <$> excFiles)
  where
  test op = op (fixPath <$> filePath) . (fixPath (addTrailingPathSeparator base) ++)

-- | Unconditionally return False.
never :: FindClause Bool
never = return False

-- | Fix a file. Only performs changes if the mode is 'Fix'. Returns
-- 'True' iff any changes would have been performed in the 'Fix' mode.

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
