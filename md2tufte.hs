#!/usr/bin/env nix-shell
#! nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (ps: [ps.text ps.turtle])"
#! nix-shell -I https://github.com/NixOS/nixpkgs/archive/20.03.tar.gz
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (FilePath)
import Turtle  hiding (input)

md2pdf :: FilePath -> FilePath
md2pdf = (<.> "pdf") . dropExtension

parser :: Parser FilePath
parser = argPath "input.md" "Markdown file to convert & typeset"

typeset :: FilePath -> FilePath -> Text
typeset = format $
  "pandoc -t latex+smart --pdf-engine=xelatex --data-dir=$HOME/.pandoc --template=tufte_template.tex --listings "
  % fp % " -o " % fp

main :: IO ExitCode
main = do
  input <- options "Convert markdown file to LaTeX & typeset" parser
  shell (typeset input $ md2pdf input) empty
