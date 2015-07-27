{-# LANGUAGE FlexibleInstances #-} -- for instance Stringish String
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

Module      : Console.Rainbow
Description : Wrapper around rainbow, adding per-line handling
Copyright   : (c) Martyn J. Pearce 2014
License     : BSD
Maintainer  : haskell@sixears.com

A module for wrapper functions around Rainbow (extended terminal handler for
colours, underline, bold, inversion, etc.); particularly around chunks of lines
(where each line is newline-free)

 -}

module Console.Rainbow
  ( CLine, CLines, Stringish(..), clines, getCLines, putLine )
where

-- base --------------------------------

import Prelude hiding ( getLine, lines )

import qualified Data.String as String

import Control.Monad  ( forM_)
import Data.List      ( intercalate )
import Data.String    ( IsString, fromString )

-- bytestring --------------------------

import qualified Data.ByteString as ByteString

import Data.ByteString  ( ByteString )

-- lens --------------------------------

import Control.Lens  ( (^.), view )

-- rainbow -----------------------------

import qualified Rainbow as R

import Rainbow        ( Renderable, putChunk )
import Rainbow.Types  ( Chunk(..), scheme, yarn )

-- text --------------------------------

import qualified Data.Text as Text

-- pragmata --------------------------------------------------------------------

-- I think concatMap reads better in here
{-# ANN module "HLint: ignore Use =<<" #-}

--------------------------------------------------------------------------------
--                                 Stringish                                  --
--------------------------------------------------------------------------------

-- | A thing that acts like a string; may have Chunks built from them, may be
--   split into lines and re-constituted with a concatenation

class IsString s => Stringish s where
  chunk      :: s -> Chunk s
  chunk      = R.chunk
  -- | split into lines
  splitLines :: s -> [s]
  -- | concat stringish things back together
  sconcat    :: [s] -> s

instance Stringish String where
  splitLines = String.lines
  sconcat    = concat

instance Stringish Text.Text where
  splitLines = Text.lines
  sconcat    = Text.concat

instance Stringish ByteString where
  splitLines = ByteString.split 10
  sconcat    = ByteString.intercalate (ByteString.pack [])

-- CLine -----------------------------------------------------------------------

-- | a chunk line that is guaranteed to be a single line (no newlines)

newtype CLine s  = CLine { getChunks  :: [Chunk s] }

instance (Show s, Stringish s) => Show (CLine s) where
  show cl = "<<" ++ (show . getLine) cl ++ ">>"

-- getLine -----------------------------

-- | extract an unadorned string from a CLine

getLine :: Stringish s => CLine s -> s
getLine = sconcat . map (view yarn) . getChunks

-- putLine -----------------------------

-- | much like 'putStrLn'; output each chunk in a CLine, followed by a newline

putLine :: Renderable s => CLine s -> IO()
putLine cl = forM_ (getChunks cl) putChunk >> putLn

-- CLines ----------------------------------------------------------------------

-- | a list of newline-free chunk lines

newtype CLines s = CLines { getCLines :: [CLine s] -- ^ extract clines
                          }
  deriving Monoid

instance (Show s, Stringish s) => Show (CLines s) where
  show cls = "[ " ++ intercalate ", " (fmap show $ getCLines cls) ++ " ]"

-- clines ------------------------------

{- | take a list of chunks, and split them into lines of chunks, while
     preserving the chunk formatting

   Examples:

   >>> clines $ map chunk ([] :: [String])
   [  ]
   >>> clines $ map chunk [ "" ]
   [ <<"">> ]
   >>> clines $ map chunk ["foo"]
   [ <<"foo">> ]
   >>> clines $ map chunk ["foo", "bar"]
   [ <<"foobar">> ]
   >>> clines $ map chunk ["foo", "bar", "\nbaz"]
   [ <<"foobar">>, <<"baz">> ]
   >>> clines $ map chunk ["foo", "bar", "\nbaz\nquux"]
   [ <<"foobar">>, <<"baz">>, <<"quux">> ]
   >>> clines $ map chunk ["foo", "bar", "\nbaz\nquux\n"]
   [ <<"foobar">>, <<"baz">>, <<"quux">> ]
   >>> clines $ map chunk ["foo", "bar", "\nbaz\nquux\n\n"]
   [ <<"foobar">>, <<"baz">>, <<"quux">>, <<"">> ]

   >>> clines $ map chunk ["foo", "bar", "\nbaz\nquux"]
   [ <<"foobar">>, <<"baz">>, <<"quux">> ]


   >>> :m + Data.Text
   >>> clines $ Prelude.map (chunk . pack) ["foo\nbar"]
   [ <<"foo">>, <<"bar">> ]

   >>> :m - Data.Text
   >>> :m + Data.ByteString
   >>> :set -XOverloadedStrings
   >>> let s = "foo\nbar" :: ByteString
   >>> clines $ Prelude.map chunk [s]
   [ <<"foo">>, <<"bar">> ]
 -}

clines :: Stringish s => [Chunk s] -> CLines s
clines = CLines . clines'
         where -- take a list of chunks, split embedded chunks at newlines to
               -- produce a non-empty list of clines
               clines' :: Stringish s => [Chunk s] -> [CLine s]
               clines' cs = fmap (CLine . reverse) . reverse $ foldl f [] cs
               -- given a list of proto-clines (each [Chunk s] will be a
               -- proto-cline), and another chunk, split the chunk as necessary;
               -- add the pieces to the existing proto-cline or form new
               -- cline(s) as appropriate
               f :: Stringish s => [[Chunk s]] -> Chunk s -> [[Chunk s]]
               f a c = case a of
                           [] -> fmap (:[]) . reverse $ lines' c
                           a' : as ->
                             case lines' c of
                               []      -> -- lines' is guaranteed to never
                                          -- return the empty list
                                          error "lines' gave []!"
                               [x]     -> -- a single-element list; no line breaks
                                          (x : a') : as
                               l' : ls -> concat [ fmap (:[]) (reverse ls)
                                                 , [l' : a']
                                                 , as ]

--------------------------------------------------------------------------------

-- putLn -------------------------------

-- | write a newline to stdout

putLn :: IO ()
putLn = putStrLn ""

-- lines -------------------------------

-- | just for testing; take some chunks, split them into lines and extract the 
--   resulting Stringish things
_linesc :: Stringish s => [Chunk s] -> [s]
_linesc = (concatMap . fmap) (view yarn) . fmap getChunks . getCLines . clines

-- | just for testing; _lines produces a single empty list from a single empty list
--   where Prelude.lines produces an empty list (indistinguishable from lines [])
--
-- prop> (not . null) s ==> (fmap chunk . _lines) [s] == lines (chunk s)
_lines :: [String] -> [String]
_lines = concatMap l
         where l "" = [ "" ]
               l s  = String.lines s

{- | split a single chunk into a list of chunks, one per line

   prop> _lines ss == _linesc (fmap chunk ss)
 -}

lines :: Stringish s => Chunk s -> [Chunk s]
lines c =
  let schm = c ^. scheme
  in fmap (Chunk schm) $ splitLines (c ^. yarn)

-- lines' ------------------------------

-- | like lines, but an empty chunk will produce a list of a single chunk (with
--   an empty string) rather than an empty list
lines' :: Stringish s => Chunk s -> [Chunk s]
lines' c = case lines c of
             [] -> [ chunk (fromString "") ]
             ls -> ls
