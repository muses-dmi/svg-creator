{-# LANGUAGE OverloadedStrings #-}
{-# language ExtendedDefaultRules #-}
{-# language OverloadedLabels #-}

-- the following command will generate the SVG and pretty print it
-- stack run |  xmllint --format -

module Demo where

import Data.Text hiding (map, mconcat, foldr, print)
import Graphics.Svg

import Graphics.Svg.Controller

import Named ((!))

-- make Int and then float the default for numbers
default (Int, Float)

gen :: IO ()
gen = do
  writeFile name $ show $ interfaceNoBorder lightblock interface 
 -- print $ interfaceNoBorder lightblock interface

name = "examples/example1.svg"
lightpad = pad ! #x 0 ! #y 0 ! #size 3 
               ! #address "/midicc" # iargs [100]
               # fill "rgb(217,137,188)"

interface = lightpad