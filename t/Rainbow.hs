#!/usr/local/bin/runghc -i/home/martyn/bin/hlib:/home/martyn/src/rainbow-lines/src

module Fluffy.Console.T.Rainbow where

-- base --------------------------------

import Prelude hiding ( lines )

-- Fluffy ------------------------------

import Fluffy.Devel.Test  ( diag, is, okay, test )

-- this module ---------------------------------------------

import Console.Rainbow  ( cpack, clines, lines, lines' )


--------------------------------------------------------------------------------

main = test [ is (lines (cpack "foo\nbar")) (fmap cpack ["foo", "bar"])
                                                                  "simple lines"
            , is (lines (cpack "foo\nbar\n")) (fmap cpack ["foo", "bar"])
                                                                "simple lines 2"
            , is (lines (cpack "foo")) [cpack "foo"]        "super simple lines"
            , is (lines (cpack "\n")) [cpack ""]                "one line lines"
            , is (lines (cpack "")) []                           "no line lines"

            , is (lines' (cpack "foo\nbar")) (fmap cpack ["foo", "bar"])
                                                                 "simple lines'"
            , is (lines' (cpack "foo\nbar\n")) (fmap cpack ["foo", "bar"])
                                                               "simple lines' 2"
            , is (lines' (cpack "foo")) [cpack "foo"]      "super simple lines'"
            , is (lines' (cpack "\n")) [cpack ""]              "one line lines'"
            , is (lines' (cpack "")) [cpack ""]                 "no line lines'"
            , is (show $ clines [fmap cpack [ "foo\nbar", "baz"
                                               , "quux\nfortuna\n" ]])
                    "[ <<foo>>, <<barbazquux>>, <<fortuna>> ]"
                                                                         "cline"
               ]
