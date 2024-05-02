{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Lib
    ( someFunc
    ) where

import Data.Aeson (ToJSON, FromJSON, eitherDecode, encode)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString.Lazy as B
import Data.Char
import Data.Text
import qualified Data.Text.IO as I
import GHC.Generics
import System.Directory
import System.IO

someFunc :: IO ()
someFunc = putStrLn "someFunc"
