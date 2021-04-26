-- |
-- Module      : Verismith.Verilog.Preprocess
-- Description : Simple preprocessor for `define and comments.
-- Copyright   : (c) 2011-2015 Tom Hawkins, 2019 Yann Herklotz
-- License     : GPL-3
-- Maintainer  : yann [at] yannherklotz [dot] com
-- Stability   : experimental
-- Portability : POSIX
--
-- Simple preprocessor for `define and comments.
--
-- The code is from https://github.com/tomahawkins/verilog.
--
-- Edits to the original code are warning fixes and formatting changes.
module Verismith.Verilog.Preprocess
  ( uncomment,
    preprocess,
  )
where

-- | Remove comments from code. There is no difference between @(* *)@ and
-- @/* */@, therefore in this implementation, @*/@ could close @(*@ and vice-versa,
-- This will be fixed in an upcoming version.
uncomment :: FilePath -> String -> String
uncomment file = uncomment'
  where
    uncomment' a = case a of
      "" -> ""
      '/' : '/' : rest -> "  " ++ removeEOL rest
      '/' : '*' : rest -> "  " ++ remove rest
      '(' : '*' : ')' : rest -> '(' : '*' : ')' : rest ++ remove rest
      '(' : '*' : rest -> "  " ++ remove rest
      '"' : rest -> '"' : ignoreString rest
      b : rest -> b : uncomment' rest
    removeEOL a = case a of
      "" -> ""
      '\n' : rest -> '\n' : uncomment' rest
      '\t' : rest -> '\t' : removeEOL rest
      _ : rest -> ' ' : removeEOL rest
    remove a = case a of
      "" -> error $ "File ended without closing comment (*/): " ++ file
      '"' : rest -> removeString rest
      '\n' : rest -> '\n' : remove rest
      '\t' : rest -> '\t' : remove rest
      '*' : '/' : rest -> "  " ++ uncomment' rest
      '*' : ')' : rest -> "  " ++ uncomment' rest
      _ : rest -> " " ++ remove rest
    removeString a = case a of
      "" -> error $ "File ended without closing string: " ++ file
      '"' : rest -> " " ++ remove rest
      '\\' : '"' : rest -> "  " ++ removeString rest
      '\n' : rest -> '\n' : removeString rest
      '\t' : rest -> '\t' : removeString rest
      _ : rest -> ' ' : removeString rest
    ignoreString a = case a of
      "" -> error $ "File ended without closing string: " ++ file
      '"' : rest -> '"' : uncomment' rest
      '\\' : '"' : rest -> "\\\"" ++ ignoreString rest
      b : rest -> b : ignoreString rest

-- | A simple `define preprocessor.
preprocess :: [(String, String)] -> FilePath -> String -> String
preprocess env file content =
  unlines $ pp True [] env $ lines $
    uncomment
      file
      content
  where
    pp :: Bool -> [Bool] -> [(String, String)] -> [String] -> [String]
    pp _ _ _ [] = []
    pp on stack env_ (a : rest) = case words a of
      "`define" : name : value ->
        ""
          : pp
            on
            stack
            ( if on
                then (name, ppLine env_ $ unwords value) : env_
                else env_
            )
            rest
      "`ifdef" : name : _ ->
        "" : pp (on && elem name (map fst env_)) (on : stack) env_ rest
      "`ifndef" : name : _ ->
        "" : pp (on && notElem name (map fst env_)) (on : stack) env_ rest
      "`else" : _
        | not $ null stack ->
          "" : pp (head stack && not on) stack env_ rest
        | otherwise ->
          error $ "`else  without associated `ifdef/`ifndef: " ++ file
      "`endif" : _
        | not $ null stack ->
          "" : pp (head stack) (tail stack) env_ rest
        | otherwise ->
          error $ "`endif  without associated `ifdef/`ifndef: " ++ file
      "`timescale" : _ -> pp on stack env_ rest
      _ -> (if on then ppLine env_ a else "") : pp on stack env_ rest

ppLine :: [(String, String)] -> String -> String
ppLine _ "" = ""
ppLine env ('`' : a) = case lookup name env of
  Just value -> value ++ ppLine env rest
  Nothing -> error $ "Undefined macro: `" ++ name ++ "  Env: " ++ show env
  where
    name =
      takeWhile
        (flip elem $ ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ ['_'])
        a
    rest = drop (length name) a
ppLine env (a : b) = a : ppLine env b
