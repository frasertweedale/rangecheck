-- This file is part of rangecheck
-- Copyright (C) 2020  Red Hat, Inc.
--
-- purebred-email is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

import Control.Monad (join, when)
import Data.Foldable (for_, traverse_)
import Data.Function (on)
import Data.List (groupBy, sort)
import System.Environment

import Control.Monad.State
import qualified Data.Text.Lazy.IO as T
import System.FilePath (takeFileName)

import Rangecheck

main :: IO ()
main = do
  args <- getArgs
  ranges <- join <$> traverse extractRangesFromFile args
  let rangesByType = groupBy ((==) `on` fst) . sort $ ranges
  traverse_ analyseRanges rangesByType

-- | Assumption: ranges are all of same 'RangeType', AND ranges are
-- sorted in order of start value
analyseRanges :: [(RangeType, AnnotatedRange String)] -> IO ()
analyseRanges [] = pure ()
analyseRanges xs = do
  putStr $ "RANGE TYPE: " <> show (fst (head xs)) <> "\n\n"
  flip evalStateT (0, 0) . for_ xs $ \(_, range@((lo, hi), ann)) -> do
    liftIO $ print range
    (lastLo, lastHi) <- get
    put (lo, hi)
    when (lo <= lastHi) $ do
      liftIO $ putStrLn "  RANGE OVERLAP!"
      put (min lastLo lo, max lastHi hi)
    when (hi <= lo) $
      liftIO $ putStrLn "  EMPTY RANGE!"
  putStr "\n\n"
  
extractRangesFromFile :: FilePath -> IO [(RangeType, AnnotatedRange String)]
extractRangesFromFile fp = do
  cfg <- T.readFile fp
  pure . annotateRanges (takeFileName fp) . extractRanges . buildMap $ cfg
