{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Blank                     -- import the blank canvas
import Data.Complex
import System.Random
import Data.Text (Text)
import qualified Data.Text as Text

type Coord = (Double,Double)
type Color = Text

drawCircle :: (Double, Double, Double) -> Color -> Canvas ()
drawCircle (x, y, r) color = do
    beginPath()
    arc(x, y, r, 0, 2 * pi, False)
    lineWidth  (r*2 / 125)
    strokeStyle color
    stroke()

drawDot :: Coord -> Color -> Canvas ()
drawDot (x, y) color = do
    beginPath()
    arc(x, y, 0.01, 0, 2 * pi, False)
    lineWidth  0.01--(r*2 / 125)
    
    strokeStyle color
    stroke()
    fillStyle color
    fill ()

drawSmallLine :: (Coord, Coord) -> Color -> Canvas ()
drawSmallLine ((x1,y1), (x2,y2)) color = do
    beginPath ()
    moveTo (x1,y1)
    lineTo (x2,y2)
    lineWidth 0.1
    strokeStyle color
    stroke ()

drawLine :: (Coord, Coord) -> Color -> Canvas ()
drawLine ((x1,y1), (x2,y2)) color = do
    beginPath ()
    moveTo (x1,y1)
    lineTo (x2,y2)
    lineWidth 1
    strokeStyle color
    stroke ()

drawTriangle :: Coord -> Color -> Canvas ()
drawTriangle (x,y) color = do
    beginPath()
    moveTo(x,y)
    lineTo(x-2.5,y+4)
    lineTo(x+2.5,y+4)
    fillStyle color
    fill()


drawAxis :: Coord -> Canvas ()
drawAxis (w, h) = do
    moveTo(w/2,0)
    lineTo(w/2,h)
    moveTo(0,h/2)
    lineTo(w,h/2)
    lineWidth 2
    strokeStyle "red"
    stroke()

--(x,y,r), count
circleFractal :: (Double,Double,Double) -> Int -> [((Double,Double,Double), Int)]
circleFractal (centerX,centerY,r) 0 = [((centerX,centerY,r), 0)]
circleFractal (centerX,centerY,r) count = [((centerX,centerY,r), count)]
     ++ (circleFractal (centerX + r, centerY, r/2) (count - 1))
     ++ (circleFractal (centerX, centerY + r, r/2) (count - 1))
     ++ (circleFractal (centerX, centerY - r, r/2) (count - 1))
     ++ (circleFractal (centerX - r, centerY, r/2) (count - 1))

--(x,Y), length, angle, depth/count, current iter
treeFractal :: Coord -> Double -> Double -> Int -> Int -> [(Coord,Coord)]
treeFractal (x,y) l a 0  iter   = do
      let x1 = x
          y1 = y
          x2 = x-l*cos (a + (a * fromIntegral(iter))) 
          y2 = y-l*sin (a + (a * fromIntegral(iter)))
          x3 = x-l*cos ( (a - (a * fromIntegral(iter)))) 
          y3 = y+l*sin ( (a - (a * fromIntegral(iter)))) 
      [((x1,y1),(x2,y2)), ((x1,y1),(x3,y3))]
treeFractal (x,y) l a count iter = do
      let x1 = x
          y1 = y
          x2 = x-l*cos (a + (a * fromIntegral(iter))) 
          y2 = y-l*sin (a + (a * fromIntegral(iter))) 
          x3 = x-l*cos ( (a - (a * fromIntegral(iter)))) 
          y3 = y+l*sin ( (a - (a * fromIntegral(iter)))) 
      [((x1,y1),(x2,y2)), ((x1,y1),(x3,y3))]
        ++ treeFractal (x2,y2) (0.66*l) a (count - 1) (iter+1)
        ++ treeFractal (x3,y3) (0.66*l) a (count - 1) (iter+1)

--Psuedo matrix multiplication
fernFractal1 :: Coord -> [Double] -> [Coord]
fernFractal1 _ [] = []
fernFractal1 (x, y) rand = do
      if (head rand <= 1)
        then do 
          let x1 = 0
              y1 = 0.16*y
          [(x1,y1)] ++ fernFractal1 (x1, y1) (tail rand)
        else if (head rand <= 86)
          then do 
            let x1 = (0.85*x + 0.04*y)
                y1 = ((-0.04)*x + 0.85*y + 1.6)
            [(x1,y1)] ++ fernFractal1 (x1, y1) (tail rand)
          else if (head rand <= 93)
            then do 
              let x1 = (0.2*x - 0.26*y)
                  y1 = (0.23*x + 0.22*y + 1.6)
              [(x1,y1)] ++ fernFractal1 (x1, y1) (tail rand)
            else do
              let x1 = ((-0.15)*x + 0.28*y)
                  y1 = (0.26*x + 0.24*y + 0.44)
              [(x1,y1)] ++ fernFractal1 (x1, y1) (tail rand)

fernFractal2 :: Coord -> [Double] -> [Coord]
fernFractal2 _ [] = []
fernFractal2 (x, y) rand = do
      if (head rand <= 2)
        then do 
          let x1 = 0
              y1 = 0.25*y - 0.4
          [(x1,y1)] ++ fernFractal2 (x1, y1) (tail rand)
        else if (head rand <= 86)
          then do 
            let x1 = (0.95*x + 0.005*y - 0.002)
                y1 = ((-0.005)*x + 0.93*y + 0.5)
            [(x1,y1)] ++ fernFractal2 (x1, y1) (tail rand)
          else if (head rand <= 93)
            then do 
              let x1 = (0.035*x - 0.2*y - 0.09)
                  y1 = (0.16*x + 0.04*y + 0.02)
              [(x1,y1)] ++ fernFractal2 (x1, y1) (tail rand)
            else do
              let x1 = ((-0.04)*x + 0.2*y + 0.083)
                  y1 = (0.16*x + 0.04*y + 0.12)
              [(x1,y1)] ++ fernFractal2 (x1, y1) (tail rand)

triforce :: Coord -> Int -> [Coord]
triforce _ 0 = []
triforce (x,y) num = do
        let x' = x + (10*(fromIntegral(num)-1))
            y' = y
        --rotate pi
        [(x',y'), (x'-2.5,y'+4), (x'+2.5,y'+4)] ++ (triforce (x,y) (num -1))

sierpinkskiTri :: Coord -> Int -> [Coord]
sierpinkskiTri _ 0 = []
sierpinkskiTri (x,y) depth = --do
        (triforce (x,y) depth) ++ (sierpinkskiTri (x+5,y-8) (depth-1))

main ::  IO ()
main = do
--    c <- getLine
    blankCanvas 3000 {events = ["keydown","mousedown"] } $ \ context -> do
          loop context

loop context = do 
      let (w,h) = (width context, height context)
          depth = 8-- only used for circle, tree and sierpinkski

      send context $ do
          beginPath()
          font "30pt Calibri"
          fillStyle "black"
          fillText("Circle Fractal" ,100,50)
          fillStyle "blue"
          fillText("Binary Tree 'Fractal'" ,100,150)
          fillStyle "green"
          fillText("Fern Fractal #1" ,100,250)
          fillStyle "red"
          fillText("Fern Fractal #2" ,100,350)
          fillStyle "purple"
          fillText("Sierpinkski Triangle (Not fully working)" ,100,450)
      event <- wait context
      case ePageXY event of
           Nothing -> loop context
           Just (x',y') -> do send context $ do
                                clearRect(0,0,w,h)
                                translate(w/2,h/2)
                                scale(w/24,h/24)
                              fractal_func context f [] depth
                                  where f | y' <= 100             = "circleFractal"
                                          | y' > 100 && y' <= 185 = "treeFractal"
                                          | y' > 185 && y' <= 290 = "fernFractalA"
                                          | y' > 290 && y' <= 380 = "fernFractal2"
                                          | y' > 380              = "sierpinkskiTri"

fractal_func :: DeviceContext -> String -> [Int] -> Int -> IO b
fractal_func context fract action depth = do
      g <- getStdGen
      send context $ do
          let (w,h) = (width context, height context)
              zoom = depth
              randNums = map (*100) (take 7500 (randoms g ::[Double]))
          redraw (w,h) action
          --Meet the fractals
          --This just looks nice
          case fract of "circleFractal" -> do
                            sequence_ (map ((flip drawCircle) "black" . fst) (circleFractal (0,0,25) zoom))
          --Tree Fractal (1 Line to 2, 2 to 4...)
                        "treeFractal" -> do
                            sequence_ (map ((flip drawSmallLine) "blue") (treeFractal (0,0) 25 (pi/8) zoom 0))
          --Fern Fractal relies on random generated numbers
                        "fernFractalA" -> do
                            sequence_ (map ((flip drawDot) "green") (fernFractal1 (0,0) randNums) )
                        "fernFractal2" -> do
                            sequence_ (map ((flip drawDot) "red") (fernFractal2 (0,0) randNums) )
          --Sierpinkski Triangle: does not display exactly like a Sierpinkski Triangle...
                        "sierpinkskiTri" -> do
                            sequence_ (map ((flip drawTriangle) "purple") (sierpinkskiTri (0,0) zoom))

      event <- wait context
      let key = case (eType event, eWhich event) of
                ("keydown",Just c) -> [c]
                _ -> []
      let current_key = [ k | k <- key]
      fractal_func context fract current_key depth 

{-
=/+ = 187
-/_ = 189
left = 37
up = 38
right = 39
down = 40
-}
redraw :: Coord -> [Int] -> Canvas ()
redraw (w,h) action = do
    clearRect(0,0,w,(-h))
    clearRect(0,0,(-w),h)
    clearRect(0,0,w,h)
    clearRect(0,0,(-w),(-h))
    case action of
        [189] -> scale(0.5,0.5)     --zoom out
        [187] -> scale(2,2)         --zoom in
        [37] -> translate((-2), 0) --move left
        [38] -> translate(0, 2)    --move up
        [39] -> translate(2,0)     --move right
        [40] -> translate(0,(-2))  --move down
        _    -> scale(1,1)


{-
--Mandlebrot set does not work, need to plot imaginary numbers and really big numbers
--Being able to do a Mandlebrot set will unlock a lot of other fractals
--mandlebrot :: Complex Double -> Complex Double -> [(Double,Double,Double)]
mandlebrot x c = do
let x' = x^2 + c
xy =  x'
if magnitude x' > 200
then []
else [(realPart xy, imagPart xy, 0.1)]
++ (mandlebrot x' c)

sequence_ (map drawCircle (mandlebrot (0 :+ 0) ((0.285) :+ 0) ))

MandelBrot
c = a + b*i
f(0) = 0^2 + c
f(1) = c^2 + c
f(2) = (c^2 + c) + c
...
f(z) = f(z-1)^2 + c
f(z+1) = f(z)^2 + c
-}
