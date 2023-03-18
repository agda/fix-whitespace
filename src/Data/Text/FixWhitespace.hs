
{-# LANGUAGE OverloadedStrings #-}

module Data.Text.FixWhitespace
  ( CheckResult(..)
  , checkFile
  , LineError(..)
  , displayLineError
  , transform
  , transformWithLog
  , TabSize
  , Verbose
  , defaultTabSize
  )
  where

import           Control.Monad                     ( (<=<) )
import           Control.Monad.Trans.Writer.Strict ( Writer, runWriter, tell )
import           Control.Exception                 ( IOException, handle )

import           Data.Char                         ( GeneralCategory(Space, Format), generalCategory )
import           Data.Text                         ( Text )
import qualified Data.Text                         as Text
import qualified Data.Text.IO                      as Text  {- Strict IO -}

import           System.IO                         ( IOMode(ReadMode), hSetEncoding, utf8, withFile )

import           Data.List.Extra.Drop              ( dropWhileEnd1, dropWhile1 )

type Verbose = Bool
type TabSize = Int

-- | Default tab size.
--
defaultTabSize :: TabSize
defaultTabSize = 8

-- | Result of checking a file against the whitespace policy.
--
data CheckResult
  = CheckOK
      -- ^ The file satifies the policy.
  | CheckViolation Text [LineError]
      -- ^ The file violates the policy, a fix and a list of
      --   violating lines are returned.
  | CheckIOError IOException
      -- ^ An I/O error occurred while accessing the file.
      --   (E.g., the file is not UTF8 encoded.)

-- | Represents a line of input violating whitespace rules.
--   Stores the index of the line and the line itself.
data LineError = LineError Int Text

-- | Check a file against the whitespace policy,
--   returning a fix if violations occurred.
--
checkFile :: TabSize -> Verbose -> FilePath -> IO CheckResult
checkFile tabSize verbose f =
  handle (\ (e :: IOException) -> return $ CheckIOError e) $
    withFile f ReadMode $ \ h -> do
      hSetEncoding h utf8
      s <- Text.hGetContents h
      let (s', lvs)
            | verbose   = transformWithLog tabSize s
            | otherwise = (transform tabSize s, [])
      return $ if s' == s then CheckOK else CheckViolation s' lvs

transform
  :: TabSize   -- ^ Expand tab characters to so many spaces.  Keep tabs if @<= 0@.
  -> Text      -- ^ Text before transformation.
  -> Text      -- ^ Text after transformation.
transform tabSize =
  Text.unlines .
  removeFinalEmptyLinesExceptOne .
  map (removeTrailingWhitespace .  convertTabs tabSize) .
  Text.lines
  where
  removeFinalEmptyLinesExceptOne =
    reverse . dropWhile1 Text.null . reverse

-- | The transformation monad: maintains info about lines that
--   violate the rules. Used in the verbose mode to build a log.
--
type TransformM = Writer [LineError]

-- | Transforms the contents of a file.
--
transformWithLog
  :: TabSize             -- ^ Expand tab characters to so many spaces.  Keep tabs if @<= 0@.
  -> Text                -- ^ Text before transformation.
  -> (Text, [LineError]) -- ^ Text after transformation and violating lines if any.
transformWithLog tabSize =
  runWriter .
  fmap Text.unlines .
  fixAllViolations .
  zip [1..] .
  Text.lines
  where
  fixAllViolations :: [(Int,Text)] -> TransformM [Text]
  fixAllViolations =
    removeFinalEmptyLinesExceptOne
    <=<
    mapM (fixLineWith $ removeTrailingWhitespace . convertTabs tabSize)

  removeFinalEmptyLinesExceptOne :: [Text] -> TransformM [Text]
  removeFinalEmptyLinesExceptOne ls
    | lenLs == lenLs' = pure ls
    | otherwise       = do
        tell $ zipWith LineError [1+lenLs' ..] els
        pure ls'
    where
    ls'    = dropWhileEnd1 Text.null ls
    lenLs  = length ls
    lenLs' = length ls'
    els    = replicate (lenLs - lenLs') ""

  fixLineWith :: (Text -> Text) -> (Int, Text) -> TransformM Text
  fixLineWith fixer (i, l)
    | l == l'   = pure l
    | otherwise = do
        tell [LineError i l]
        pure l'
    where
    l' = fixer l

removeTrailingWhitespace :: Text -> Text
removeTrailingWhitespace =
  Text.dropWhileEnd $ \ c -> generalCategory c `elem` [Space,Format] || c == '\t'

convertTabs :: TabSize -> Text -> Text
convertTabs tabSize = if tabSize <= 0 then id else
  Text.pack . reverse . fst . foldl (convertOne tabSize) ([], 0) . Text.unpack

convertOne :: TabSize -> (String, Int) -> Char -> (String, Int)
convertOne tabSize (a, p) '\t' = (addSpaces n a, p + n)
  where
  n = tabSize - p `mod` tabSize  -- Here, tabSize > 0 is guaranteed
convertOne _tabSize (a, p) c = (c:a, p+1)

addSpaces :: Int -> String -> String
addSpaces n = (replicate n ' ' ++)

-- | Print a erroneous line with 'visibleSpaces'.
--
displayLineError :: FilePath -> LineError -> Text
displayLineError fname (LineError i l) = Text.concat
  [ Text.pack fname
  , ":"
  , Text.pack $ show i
  , ": "
  , visibleSpaces l
  ]

-- | Replace spaces and tabs with visible characters for presentation purposes.
--   Space turns into '·' and tab into '<TAB>'.
--
visibleSpaces :: Text -> Text
visibleSpaces s
  | Text.null s = "<NEWLINE>"
  | otherwise = flip Text.concatMap s $ \case
      '\t' -> "<TAB>"
      ' '  -> "·"
      c    -> Text.pack [c]
