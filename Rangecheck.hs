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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Rangecheck where

import Prelude hiding (lookup)
import Control.Monad ((=<<))
import Data.Maybe (catMaybes)
import Data.Function (on)
import Numeric (readHex)

import Data.Map
import qualified Data.Text.Lazy as T
import Safe (readMay)

type Range = (Int, Int)
type AnnotatedRange a = (Range, a)

data Radix = Dec | Hex

data RangeType = Serial | Request | Replica
  deriving (Show, Eq, Ord)

serialLo, serialHi, serialNextLo, serialNextHi
  , requestLo, requestHi, requestNextLo, requestNextHi
  , replicaLo, replicaHi, replicaNextLo, replicaNextHi :: T.Text
serialLo      = "dbs.beginSerialNumber"
serialHi      = "dbs.endSerialNumber"
serialNextLo  = "dbs.nextBeginSerialNumber"
serialNextHi  = "dbs.nextEndSerialNumber"
requestLo     = "dbs.beginRequestNumber"
requestHi     = "dbs.endRequestNumber"
requestNextLo = "dbs.nextBeginRequestNumber"
requestNextHi = "dbs.nextEndRequestNumber"
replicaLo     = "dbs.beginReplicaNumber"
replicaHi     = "dbs.endReplicaNumber"
replicaNextLo = "dbs.nextBeginReplicaNumber"
replicaNextHi = "dbs.nextEndReplicaNumber"

keysOfInterest :: [T.Text]
keysOfInterest = 
  [ serialLo, serialHi, serialNextLo, serialNextHi
  , requestLo, requestHi, requestNextLo, requestNextHi
  , replicaLo, replicaHi, replicaNextLo, replicaNextHi
  ]

buildMap :: T.Text -> Map T.Text T.Text
buildMap = foldMap (f . T.break (== '='))  . T.lines
  where
  f (_, "") = mempty
  f (k, v) | k `elem` keysOfInterest = singleton k (T.tail v)
  f _ = mempty

parseNumber :: Radix -> String -> Maybe Int
parseNumber Dec = readMay
parseNumber Hex = \s -> case readHex s of ((n, ""):_) -> Just n ; _ -> Nothing

extractRange :: Radix -> T.Text -> T.Text -> Map T.Text T.Text -> Maybe Range
extractRange r kLo kHi m = do
  lo <- parseNumber r . T.unpack =<< lookup kLo m
  hi <- parseNumber r . T.unpack =<< lookup kHi m
  pure (lo, hi)

-- | Extract ranges from a CS.cfg
extractRanges :: Map T.Text T.Text -> [(RangeType, Range)]
extractRanges m = catMaybes
  [ fmap (Serial,) (extractRange Hex serialLo serialHi m)
  , fmap (Serial,) (extractRange Hex serialNextLo serialNextHi m)
  , fmap (Request,) (extractRange Dec requestLo requestHi m)
  , fmap (Request,) (extractRange Dec requestNextLo requestNextHi m)
  , fmap (Replica,) (extractRange Dec replicaLo replicaHi m)
  , fmap (Replica,) (extractRange Dec replicaNextLo replicaNextHi m)
  ]

annotateRanges :: s -> [(a, b)] -> [(a, (b, s))]
annotateRanges s = fmap (fmap (,s))
