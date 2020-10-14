#!/usr/bin/env nix-shell
#! nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (ps: [ps.text ps.turtle])"
#! nix-shell -I https://github.com/NixOS/nixpkgs/archive/20.03.tar.gz
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Bifunctor (second, bimap)
import           Data.Foldable  (traverse_)
import qualified Data.Text      as T
import           Prelude        hiding (FilePath)
import           Turtle         hiding (d, proc, symlink)

type FileLink = (FilePath, FilePath)

dotfiles :: IO FilePath
dotfiles = (</> "github/dotfiles") <$> home

config :: IO FilePath
config = (</> "config.yaml") <$> dotfiles

toFL :: Text -> FileLink
toFL =
  bimap fromText fromText
    . second (T.dropWhile ((||) <$> (== ':') <*> (== ' ')))
    . T.break (== ':')

fileLinks :: Text -> [FileLink]
fileLinks = fmap toFL . T.lines

symlink :: FileLink -> IO ()
symlink (file, link) = do
  h <- home
  d <- dotfiles
  procs "ln" ["-s", proc (d </> file), proc (h </> link)] empty
  where proc = either (error . T.unpack) id . toText

main :: IO ()
main = do
  echo "Setting up…"
  configFile <- config
  configText <- readTextFile configFile
  traverse_ symlink $ fileLinks configText
  echo "Done!"
