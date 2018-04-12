import Data.Array
import Graphics.UI.GLUT

width = 2
height = 2

data GameBoard = GameBoard (Array (Int, Int) Bool)

arrayChange f (GameBoard a) = GameBoard $ f a

setTile x y = arrayChange (// [((x,y), True)])

instance Show GameBoard where
  show (GameBoard a) = init $ concatMap (\y -> (map (\x -> head $ show $ a ! (x,height-y-1)) [0..w])++"\n") [0..h]
    where ((0,0),(w,h)) = bounds a

emptyBoard = GameBoard $ listArray ((0,0),(width-1,height-1)) $ repeat False

fullLines (GameBoard a) = [y | y <- [0..h], all (\x -> a ! (x,y)) [0..w]]
  where ((0,0),(w,h)) = bounds a

displayBoard :: GameBoard -> IO ()
displayBoard (GameBoard a) = foldl1 (>>) [displayTile x y | x <- [0..w], y <- [0..h], a ! (x,y)]
  where ((0,0),(w,h)) = bounds a

displayTile :: Int -> Int -> IO ()
displayTile nx ny = renderPrimitive Quads $ do
  let x = realToFrac nx
      y = realToFrac ny
  vertex3f x y 0
  vertex3f x (y+1) 0
  vertex3f (x+1) (y+1) 0
  vertex3f (x+1) y 0

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World"
  displayCallback $= display
  mainLoop

color3f r g b = color $ Color3 r g (b :: GLfloat)
vertex3f x y z = vertex $ Vertex3 x y (z :: GLfloat)

display :: DisplayCallback
display = do
  clear [ColorBuffer]
  displayBoard $ setTile 0 0 emptyBoard
  flush