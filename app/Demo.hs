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

name = "examples/lightpad.svg"
lightpad = 
    mconcat (map p [(0,100), (4, 101), (8, 102), (12, 103)])
    <>
    mconcat (map s [(0,104), (4, 105), (8, 106), (12, 107)])
    where
      p = \(x, cc) -> pad ! #x x ! #y 0 ! #size 3 ! #address "/midicc" # iargs [cc]
                                                                       # fill "rgb(217,137,188)"
      s = \(x, cc) -> 
        vertSlider ! #x x ! #y 4
                   ! #width 3 ! #height 11 ! #min 0 ! #max 127 ! #address "/midicc" # fill "rgb(96,95,164)"
                                                                                    # iargs [cc]
  
interface = lightpad