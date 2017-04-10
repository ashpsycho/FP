{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-unused-binds #-}
import CodeWorld

main :: IO ()
main = exercise3

-- Fill in the blanks! (When I say blanks, I mean undefineds)

-- Exercise 1

botCircle, topCircle , midCircle :: Color -> Picture
botCircle c = colored c (translated 0 (-2.5) (solidCircle 1))
midCircle c = colored c (translated 0    0   (solidCircle 1))
topCircle c = colored c (translated 0   2.5  (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 7.5

trafficLight :: Int -> Picture
trafficLight 0  = botCircle green & midCircle black & topCircle black & frame
trafficLight 1  = botCircle black & midCircle yellow & topCircle black & frame
trafficLight 2  = botCircle black & midCircle black & topCircle red & frame
trafficLight _  = botCircle black & midCircle yellow & topCircle red   & frame

trafficController :: Double -> Picture
trafficController t
  | round (t/3) `mod` 8 <= 2 = trafficLight 0
  | round (t/3) `mod` 8 == 3 = trafficLight 1
  | round (t/3) `mod` 8 <= 6 = trafficLight 2
  | otherwise                = trafficLight 3

exercise1 :: IO ()
exercise1 = animationOf trafficController

-- Exercise 2

exercise2 :: IO ()
exercise2 = animationOf blossom
  
blossom :: Double -> Picture
blossom t = tree 8 t

tree :: Integer -> Double -> Picture
tree 0 t = blossomUp t
tree n t = path [(0,0),(0,1)] & translated 0 1 (
  rotated (pi/10) (tree (n-1) t) & rotated (- pi/10) (tree (n-1) t))

blossomUp :: Double -> Picture
blossomUp t
  | t<=0  = blank
  | t>10  = blossomUp 10
  | otherwise  =  blossomUp (fromIntegral (floor (t-0.001))) & colored (getColor (ceiling t)) (translated 0 (t/30) (solidCircle (t/30)))

colors = [yellow, yellow, yellow, yellow, white, white, pink, pink, red, red ]

getColor :: Int -> Color
getColor t
  | t<=0  = white
  | t>=10 = pink
  | otherwise = head (drop t colors)

-- Exercise 3

wall, ground, storage, box :: Picture
wall =     foldr (&) ( colored (grey 0.4) (solidRectangle 1 1)) (map (\(x,y) -> translated x y (rectangle 0.5 0.333)) [(0.25,0),(-0.25,0),(0,0.333),(0,-0.333)] )
ground =   foldr (&) (colored yellow (solidRectangle 1 1)) (map (\(x,y) -> translated (x/5) (y/5) (solidCircle 0.02) ) (getAllPositions 2))
storage  = colored red (path [(-0.2,-0.2), (0.2,0.2)]) & colored red (path [(-0.2,0.2), (0.2,-0.2)]) & ground 
box =      (path [(0.35,0.35),(-0.35,-0.35)]) & (path [(-0.35,0.35),(0.35,-0.35)]) & (rectangle 1 1) & (rectangle 0.7 0.7) & colored brown (solidRectangle 1 1) 

tiles = [blank,wall, ground, storage, box];

drawTile :: Int -> Picture
drawTile n
  | n<0 || n>4 = blank
  | True  = head( drop n tiles)
         
pictureOfMaze :: Picture
pictureOfMaze = foldr (&) blank (map (\(x,y) -> translated x y (drawTile (maze (round x) (round y)))) (getAllPositions 10))
  
getAllPositions n = [(x,y) | x<-[(-1*n)..n],y <-[(-1*n)..n]]

exercise3 :: IO ()
exercise3 = drawingOf pictureOfMaze
         
maze :: Integer -> Integer -> Int 
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2
 