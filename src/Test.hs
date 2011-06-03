{-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances #-}

import            Test.QuickCheck (Arbitrary(..), arbitrary, elements, listOf, listOf1, choose, quickCheck)
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8
import            Data.Bson (cast',val, Val(..))
import            Text.Printf (printf)
import            Data.List (nub, sort)

import            State

