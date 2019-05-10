{-# LANGUAGE OverloadedStrings #-}
{-# language ExtendedDefaultRules #-}
{-# language OverloadedLabels #-}

-- the following command will generate the SVG and pretty print it
-- stack run |  xmllint --format -

module Main where

import Data.Text hiding (map, mconcat, foldr, print)
import Graphics.Svg

import Graphics.Svg.Controller

import Named ((!))

-- make Int and then float the default for numbers
default (Int, Float)

---------------------------------------------------------------------------------------
-- roli lightpad example

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

---------------------------------------------------------------------------------------

mpc' = 
  (mconcat $ map (\y -> pads' (10 + y*29) 25 4 2 29) [0..3])
  <>
  stop ! #x 167 ! #y 20 ! #size 25
  <>
  play ! #v1 (162,32) ! #v2 (142,20) ! #v3 (142,45)
  <>
  record ! #x 209 ! #y 32 ! #r 12
  <>
  (mconcat $ map slider' [0..3])
  <>
  endless ! #cx 132 ! #cy 74 ! #or 14 ! #ir 7 ! #address "/mixer/pan"
  <>
  ciPad ! #x 132 ! #y 74 ! #r 5 ! #address "/mixer/mute" 
  <>
  endless ! #cx (132+30) ! #cy 74 ! #or 14 ! #ir 7 ! #address "/mixer/pan"
  <>
  ciPad ! #x (132+30) ! #y 74 ! #r 5 ! #address "/mixer/mute" 
  <>
  endless ! #cx (132+30*2) ! #cy 74 ! #or 14 ! #ir 7 ! #address "/mixer/pan"
  <>
  ciPad ! #x (132+30*2) ! #y 74 ! #r 5 ! #address "/mixer/mute" 
  <>
  endless ! #cx (132+30*3) ! #cy 74 ! #or 14 ! #ir 7 ! #address "/mixer/pan"
  <>
  ciPad ! #x (132+30*3) ! #y 74 ! #r 5 ! #address "/mixer/mute" 

slider' = \chan -> 
  vertSlider ! #x (125 + chan * 30) ! #y 90
              ! #width 15 ! #height 45 ! #min 0 ! #max 127 ! #address "/mixer/volume" 
              # iargs [chan]

pads' y size = \n x spacer -> 
    let p = \chan -> pad ! #x (x + chan*spacer) ! #y y ! #size size ! #address "/seq/chan" # iargs [chan]
    in mconcat $ foldr (\a b -> p a : b) [] [0..n-1] 


--sequencer :: Interface
sequencer = 
  pad ! #x 9 ! #y 112 ! #size 5 ! #address "/seq/chan/mute"
  <>
  pads 16 18 12
  <>
  stop ! #x 172 ! #y 51 ! #size 20
  <>
  play ! #v1 (167,61) ! #v2 (147,51) ! #v3 (147,71)
  <>
  record ! #x 207 ! #y 61 ! #r 10
  <>
  mconcat (map (sliders 1 7 25.0) [0..15])

sliders phase sep amp = 
    \i -> let xi = 10 + (i * sep)
              yi = 50 - (amp * sin (fromIntegral i / (2*pi / phase)))
          in slide i xi (round yi)

slide chan = \x y -> 
            vertSlider 
              ! #x x ! #y y
              ! #width 5 ! #height 20 ! #min 0 ! #max 127 ! #address "/mixer/volume" 
              # iargs [chan]

pads n x spacer = mconcat $ foldr (\a b -> p a : b) [] [0..n-1]
  where p = \chan -> pad ! #x (x + chan*spacer) ! #y 110 ! #size 10 ! #address "/seq/chan" # iargs [chan]

l_interface = lightpad 
s_interface = mpc'

main :: IO ()
main = do
  --print $ interfaceNoBorder lightblock l_interface
  print $ interfaceNoBorder sensel s_interface

--------------------------------------------------------------------------
-- old stuff

contents :: Element
contents =
  polygon ! #points [(475, 174), (418, 146), (418, 202), (475, 174)]
  <> circle ! #x 150 ! #y 100 ! #r 80
  <> text_   [ X_ <<- "40", Y_ <<- "20", Font_size_ <<- "12"
             , Text_anchor_ <<- "middle", Fill_ <<- "blue"] "Interface Test poo"
  <> rect ! #x 13 
           ! #y 101
           ! #height 10
           ! #width 10 # itype ipad
                       # iaddress "/seq/chan"
                       # iargs (args 0)