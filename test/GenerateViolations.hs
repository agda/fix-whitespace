-- | Generate large files with whitespace violations.
--
-- E.g. @runhaskell GenerateViolations.hs 50000@

import System.Environment

main = do
  args <- getArgs
  putStrLn $ unlines args
  let n = read $ head args
  writeFile (show n ++ "-violations.txt") $ concat $ replicate n $ " \n"
