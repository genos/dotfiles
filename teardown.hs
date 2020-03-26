#!/usr/bin/env stack
{- stack script
 --compile
 --resolver lts-15.5
 --install-ghc
 --package "text turtle"
 --ghc-options -Wall
-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Foldable (traverse_)
import qualified Data.Text     as T
import           Prelude       hiding (FilePath)
import           Turtle        hiding (f)

config :: IO FilePath
config = (</> "github/dotfiles/config.yaml") <$> home

links :: Text -> [FilePath]
links = fmap (fromText . T.tail . T.dropWhile (/= ' ')) . T.lines

unlink :: FilePath -> IO ()
unlink f = home >>= \h -> rm (h </> f)

main :: IO ()
main = do
  echo "Tearing down…"
  configFile <- config
  configText <- readTextFile configFile
  traverse_ unlink $ links configText
  echo "Done!"
