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
  writeFile name $ show $ interfaceWithBorder sensel interface 
 -- writeFile name $ show $ interfaceWithBorder landscapeA4 interface 
 -- print $ interfaceNoBorder lightblock interface

name = 