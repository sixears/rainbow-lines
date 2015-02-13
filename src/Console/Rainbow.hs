{-|

Module      : Console.Rainbow
Description : Wrapper around rainbow, adding per-line handling
Copyright   : (c) Martyn J. Pearce 2014
License     : BSD
Maintainer  : haskell@sixears.com

A module for wrapper functions around Rainbow (extend terminal handler for
colours, underline, bold, inversion, etc.); particularly around chunks of lines
(where each line is newline-free)

 -}

module Console.Rainbow
  ( CLines, CLine, clines, cpack, getCLines, lines, lines', putLine )
where

-- base ------------------------------------------------------------------------

import Prelude hiding ( getLine, lines )

import Control.Monad  ( forM_)
import Data.List      ( intercalate )

-- rainbow ---------------------------------------------------------------------

import Rainbow  ( Chunk(..), fromText, putChunk )

-- text --------------------------------

import qualified Data.Text as T

import Data.Text  ( pack, unpack )

--------------------------------------------------------------------------------

-- I think concatMap reads better in here
{-# ANN module "HLint: ignore Use =<<" #-}

-- putLn -------------------------------

-- | write a newline to stdout

putLn :: IO ()
putLn = putStrLn ""

-- CLine -----------------------------------------------------------------------

-- | a chunk line that is guaranteed to be a single line (no newlines)

newtype CLine  = CLine  { getChunks  :: [Chunk]
                        }

instance Show CLine where
  show cl = "<<" ++ getLine cl ++ ">>"

-- getLine -----------------------------

-- | extract an unadorned string from a CLine

getLine :: CLine -> String
getLine = concatMap (concatMap unpack . text) . getChunks

-- putLine -----------------------------

-- | much like 'putStrLn'; output each chunk in a CLine, followed by a newline

putLine :: CLine -> IO()
putLine cl = forM_ (getChunks cl) putChunk >> putLn

-- CLines ----------------------------------------------------------------------

-- | a list of newline-free chunk lines

newtype CLines = CLines { getCLines :: [CLine] -- ^ extract clines 
                        }

instance Show CLines where
  show cls = "[ " ++ intercalate ", " (fmap show $ getCLines cls) ++ " ]"

-- clines ------------------------------

-- | take a list of apparent lines of chunks, and produce some real lines
--   of chunks by splitting the apparent lines wherever we find a newline
clines :: [[Chunk]] -> CLines
clines = CLines . concatMap cline
         where cline :: [Chunk] -> [CLine]
               -- take a list of chunks, split embedded chunks at newlines to
               -- produce a non-empty list of clines
               cline cs = fmap (CLine . reverse) . reverse $ foldl f [] cs
                 where f a c = case a of
                                 [] -> fmap (:[]) . reverse $ lines' c
                                 a' : as ->
                                   case lines' c of
                                     []      -> -- lines is guaranteed to never
                                                -- return the empty list
                                                error "lines' gave []!"
                                     [_]     -> (c : a') : as
                                     l' : ls -> concat [ fmap (:[])
                                                              (reverse ls)
                                                       , [l' : a']
                                                       , as ]

-- cpack -------------------------------

-- | convert a String to a Chunk

cpack :: String -> Chunk
cpack = fromText . pack

-- lines -------------------------------

-- | split a single chunk into a list of chunk, one per line

lines :: Chunk -> [Chunk]
lines (Chunk { textSpec = spec, text = texts }) =
  fmap (\ t -> Chunk { textSpec = spec, text = [t] }) . T.lines $ T.concat texts

-- lines' ------------------------------

-- | like lines, but an empty chunk will produce a list of a single chunk (with
--   an empty string) rather than an empty list
lines' :: Chunk -> [Chunk]
lines' c = case lines c of
             [] -> [ cpack "" ]
             ls -> ls