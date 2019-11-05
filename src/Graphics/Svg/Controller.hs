-------------------------------------------------------------------------------
-- |
-- Module      :  SVG.Controller
-- Copyright   :  (c) 2019 Benedict R. Gaster
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  benedict.gaster@uwe.ac.uk
--
-- Slightly higher level interface utilised for controllers.
--
-------------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}
{-# language OverloadedLabels #-}
{-# language MultiParamTypeClasses #-}

module Graphics.Svg.Controller where

import Data.List.Split
import Data.List

import Data.Text

import Named hiding (with)

import Graphics.Svg.Core
import Graphics.Svg.Elements
import Graphics.Svg.Path
import Graphics.Svg.Attributes

-- diagrams like trick for applying attributes

infixl 8 #
x # f = f x

type Interface = Element

data IType = 
      IPad
    | INoteOn
    | INoteOff
    | IBorder
    | IEndless
    | IHorzSlider
    | IVertSlider
    deriving (Show, Eq)

ipad :: IType
ipad = IPad

inoteon :: IType
inoteon = INoteOn

inoteoff :: IType
inoteoff = INoteOff

iborder :: IType
iborder = IBorder

iendless :: IType
iendless = IEndless

ihorzSlider = IHorzSlider
iVertSlider = IVertSlider

itype2Attribute :: IType -> Attribute
itype2Attribute IPad        = "pad" ->> IType_
itype2Attribute INoteOn     = "noteon" ->> IType_
itype2Attribute INoteOff    = "noteoff" ->> IType_
itype2Attribute IBorder     = "border" ->> IType_
itype2Attribute IEndless    = "endless" ->> IType_
itype2Attribute IHorzSlider = "horz_slider" ->> IType_
itype2Attribute IVertSlider = "vert_slider" ->> IType_

itype :: IType -> Element -> Element
itype = flip with . (:[]) . itype2Attribute

iaddress :: Text -> Element -> Element
iaddress = flip with . (:[]) . (->>IAddress_)

fill :: Text -> Element -> Element
fill = flip with . (:[]) . (->>Fill_)

stroke :: Text -> Element -> Element
stroke = flip with . (:[]) . (->>Stroke_)

imin :: Int -> Element -> Element
imin = flip with . (:[]) . (->>Min_) . pack . show

imax :: Int -> Element -> Element
imax = flip with . (:[]) . (->>Max_) . pack . show

pressure :: Bool -> Element -> Element
pressure = flip with . (:[]) . (->>Pressure_) . pack . show

withMove :: Element -> Element
withMove = (flip with . (:[]) . (->>WithMove_) . pack) (show True)

withEnd :: Element -> Element
withEnd = (flip with . (:[]) . (->>WithEnd_) . pack) (show True)

withCoords :: Element -> Element
withCoords = (flip with . (:[]) . (->>WithCoords_) . pack) (show True)

ion :: Int -> Element -> Element
ion = flip with . (:[]) . (->>IOn_) . pack . show

ioff :: Int -> Element -> Element
ioff = flip with . (:[]) . (->>IOff_) . pack . show

class IArgsAllType t where
    iargsAll' :: [Int] -> t

instance IArgsAllType [Int] where
    iargsAll' = id

instance (IArgsAllType r) => IArgsAllType (Int -> r) where
    iargsAll' acc = \x -> iargsAll' (acc ++ [x])

args :: (IArgsAllType t) => t
args = iargsAll' []

points :: (IArgsAllType t) => t
points = args

iargs :: [Int] -> Element -> Element
iargs = flip with . (:[]) . (->>IArgs_) . args
    where
        args = Data.Text.concat . Prelude.map (pack . (++" ") . show) 

--------------------------------------------------------------------------------------
-- SVG shapes, which form the foundation for all interfaces

-- by definition these are just SVG shapes and do not define control objects, additional
-- attributes must be added for them to be well defined controls

rect :: "x" :! Int -> "y" :! Int -> "width" :! Int -> "height" :! Int -> Element
rect (Arg x) (Arg y) (Arg width) (Arg height) = 
    rect_ [ X_ <<- pack (show x), Y_ <<- pack (show y), 
            Width_ <<- pack (show width), Height_ <<- pack (show height) ] 

circle :: "x" :! Int -> "y" :! Int -> "r" :! Int -> Element
circle (Arg x) (Arg y) (Arg r) = 
    circle_ [ Cx_ <<- pack (show x), Cy_ <<- pack (show y), R_ <<- pack (show r)]

polygon :: "points" :! [(Int,Int)] -> Element
polygon (Arg points) =
    polygon_ [ Points_ <<- (pack . Data.List.intercalate " " . Prelude.map p) points]
    where p (x,y) = (show x ++ " " ++ show y)

-- intercalate " " . Data.List.Split.chunksOf 1 

--------------------------------------------------------------------------------------
-- interface for specifc kind of controllers, with fixed shapes

-- by definition these are well defined control objects

rectPad :: "x" :! Int -> "y" :! Int -> "width" :! Int -> "height" :! Int -> "address" :! Text -> Element
rectPad x y width height (Arg address) = rect x y width height # itype ipad
                                                               # iaddress address

pad :: "x" :! Int -> "y" :! Int -> "size" :! Int -> "address" :! Text -> Element
pad x y (Arg size) (Arg address) = rect x y (Arg size) (Arg size) # itype ipad
                                                                  # iaddress address

horzSlider :: "x" :! Int -> "y" :! Int -> 
          "width" :! Int -> "height" :! Int -> 
          "min" :! Int -> "max" :! Int ->
          "address" :! Text -> Element
horzSlider x y width height (Arg min) (Arg max) (Arg address) = 
    rect x y width height # itype ihorzSlider
                          # iaddress address
                          # imin min 
                          # imax max

vertSlider :: "x" :! Int -> "y" :! Int -> 
        "width" :! Int -> "height" :! Int -> 
        "min" :! Int -> "max" :! Int ->
        "address" :! Text -> Element
vertSlider x y width height (Arg min) (Arg max) (Arg address) = 
    rect x y width height # itype iVertSlider
                          # iaddress address
                          # imin min 
                          # imax max

vertSlider_ :: "x" :! Int -> "y" :! Int -> 
            "width" :! Int -> "height" :! Int -> 
            "min" :! Int -> "max" :! Int ->
            "address" :! Text -> "color" :! Text -> Element
vertSlider_ (Arg x) (Arg y) (Arg width) (Arg height) (Arg min) (Arg max) (Arg address) (Arg color) =
    vertSlider ! #x x ! #y y ! #width width ! #height height ! #min min ! #max max ! #address address
    <>
    rect ! #x (x) ! #y (y) ! #width (width) ! #height (height) # fill "none"
                                                                       # stroke color
                                                                       # itype iborder
                                                                  
ciPad :: "x" :! Int -> "y" :! Int -> "r" :! Int -> "address" :! Text -> Element
ciPad x y r (Arg address) = circle x y r # itype ipad
                                         # iaddress address

ciPad_ :: "x" :! Int -> "y" :! Int -> "r" :! Int -> "address" :! Text -> "color" :! Text -> Element
ciPad_ (Arg x) (Arg y) (Arg r) (Arg address) (Arg color) =
    ciPad ! #x x ! #y y ! #r r ! #address address 
    <>
    circle ! #x x ! #y y ! #r r  # fill "none"
                                 # stroke color
                                 # itype iborder

ciPadCor :: "x" :! Int -> "y" :! Int -> "r" :! Int -> "address" :! Text -> Element
ciPadCor (Arg x) (Arg y) (Arg r) (Arg address) = circle ! #x (x + r) ! #y (y + r) ! #r r # itype ipad
                                                                                           # iaddress address

                                                                                        
-- endless controller
-- Using this path definition as d:

-- M centerX (centerY-outerRadius)
-- A outerRadius outerRadius 0 1 0 centerX (centerY+outerRadius)
-- A outerRadius outerRadius 0 1 0 centerX (centerY-outerRadius)
-- Z
-- M centerX (centerY-innerRadius)
-- A innerRadius innerRadius 0 1 1 centerX (centerY+innerRadius)
-- A innerRadius innerRadius 0 1 1 centerX (centerY-innerRadius)
-- Z

-- draw inner and outer arcs for circle, in forward and reverse directions, as per this discussion on stack overflow:
--     https://stackoverflow.com/questions/3742479/how-to-cut-a-hole-in-an-svg-rectangle/11878784#11878784

endless :: "cx" :! Int -- centre X 
            -> "cy" :! Int -- centre y
                -> "or" :! Int -- outer radius
                    -> "ir" :! Int -- inner radius
                        -> "address" :! Text -- OSC address
                            -> Element
endless (Arg cx) (Arg cy) (Arg or) (Arg ir) (Arg address) = 
    path_ [ 
                D_ <<- pack ("M" ++ s cx ++ " " ++ s (cy - or) ++ 
                             " A" ++ s or ++ " " ++ s or ++ " 0 1 0 " ++ s cx ++ " " ++ s (cy + or) ++
                             " A" ++ s or ++ " " ++ s or ++ " 0 1 0 " ++ s cx ++ " " ++ s (cy - or) ++
                             " Z" ++
                             " M" ++ s cx ++ " " ++ s (cy - ir) ++
                             " A" ++ s ir ++ " " ++ s ir ++ " 0 1 1 " ++ s cx ++ " " ++ s (cy + ir) ++
                             " A" ++ s ir ++ " " ++ s ir ++ " 0 1 1 " ++ s cx ++ " " ++ s (cy - ir) ++
                             " Z")
          ] # itype iendless
                        # iaddress address
    where s = show

endless_ :: "cx" :! Int -- centre X 
    -> "cy" :! Int -- centre y
        -> "or" :! Int -- outer radius
            -> "ir" :! Int -- inner radius
                -> "address" :! Text -- OSC address
                    -> "color" :! Text -- Border color
                    -> Element
endless_ (Arg cx) (Arg cy) (Arg or) (Arg ir) address (Arg color) =
    endless (Arg cx) (Arg cy) (Arg or) (Arg ir) address 
    <>
    circle ! #x cx ! #y cy ! #r (or + 1) # fill "none" # stroke color # itype iborder
    <> 
    circle ! #x cx ! #y cy ! #r (ir - 1) # fill "none" # stroke color # itype iborder

radians = \degrees -> degrees * (pi/180)

-- a small set of controllers are predefined with global OSC addresses

stop :: "x" :! Int -> "y" :! Int -> "size" :! Int -> Element
stop (Arg x) (Arg y) (Arg size) = rectPad ! #x x ! #y y ! #width size ! #height size ! #address "/stop"

record :: "x" :! Int -> "y" :! Int -> "r" :! Int -> Element
record (Arg x) (Arg y) (Arg r) = ciPad ! #x x ! #y y ! #r r ! #address "/record"

play :: "v1" :! (Int,Int) -> "v2" :! (Int,Int) -> "v3" :! (Int,Int) -> Element
play (Arg v1) (Arg v2) (Arg v3) = polygon ! #points [v1,v2,v3,v1] # iaddress "/play"
                                                                  # itype ipad

horz :: "x" :! Int -> "spacer" :! Int -> [("x" :! Int -> Element)] -> Element
horz (Arg x) (Arg spacer) controls = mconcat $ Prelude.map (\(x, c) -> c ! #x x) (Prelude.zip xpos controls)
    where xpos = Prelude.map (*spacer) [0..]


-- class Horz t r where
--     horz :: t -> r

-- instance Horz ("x" :! Int -> "y" :! Int -> "size" :! Int -> "address" :! Text -> Element) r where
--     horz = error ""
border :: Interface
border = 
    rect_ [ X_ <<- "0", Y_ <<- "0", 
            Width_ <<- "240", Height_ <<- "140", 
            "blue" ->> Stroke_, "none" ->> Fill_,
            "border" ->> IType_]

-------------------------------------------------------------------------------------------------------------
-- Interface helpers

type Device = [Attribute]

landscapeA4 :: Device
landscapeA4 = [Version_ <<- "1.1", Device_ <<- "a4", Width_ <<- "297mm", Height_ <<- "210mm", ViewBox_ <<- "0 0 297 210"]

sensel :: Device
sensel = [Version_ <<- "1.1", Device_ <<- "sensel", Width_ <<- "240mm", Height_ <<- "140mm", ViewBox_ <<- "0 0 240 140"]

lightblock :: Device
lightblock = [Version_ <<- "1.1", Device_ <<- "lightpad", Width_ <<- "15mm", Height_ <<- "15mm", ViewBox_ <<- "0 0 15 15"]

interfaceNoBorder :: Device -> Interface -> Interface
interfaceNoBorder device content =
     doctype
  <> with (svg11_ content) device -- [Version_ <<- "1.1", Width_ <<- "240mm", Height_ <<- "140mm", ViewBox_ <<- "0 0 240 140"]

interfaceWithBorder :: Device -> Interface -> Interface
interfaceWithBorder device content =
    doctype
 <> with (svg11_ (border <> content)) device --[Version_ <<- "1.1", Width_ <<- "240mm", Height_ <<- "140mm", ViewBox_ <<- "0 0 240 140"]