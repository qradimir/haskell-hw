module Task.Vector where

data Vector a = Vector2D a a | Vector3D a a a

getX :: Vector a -> a
getX (Vector2D x _)   = x
getX (Vector3D x _ _) = x

getY :: Vector a -> a
getY (Vector2D _ y)   = y
getY (Vector3D _ y _) = y

getZDefault :: a -> Vector a -> a
getZDefault def (Vector2D _ _)   = def
getZDefault _   (Vector3D _ _ z) = z

toVectorDefault :: Eq a => a -> a -> a -> a -> Vector a
toVectorDefault def x y z | def == z  = Vector2D x y
                          | otherwise = Vector3D x y z

composeVectorDefault :: (Eq a) => a -> (a -> a -> a) -> Vector a -> Vector a -> Vector a
composeVectorDefault def (.+.) l r = toVector (getX l .+. getX r) (getY l .+. getY r) (getZ l .+. getZ r)
  where
    toVector = toVectorDefault def
    getZ     = getZDefault def

getZNum :: Num a => Vector a -> a
getZNum = getZDefault 0

toVectorNum :: (Eq a, Num a) => a -> a -> a -> Vector a
toVectorNum = toVectorDefault 0

composeVectorNum :: (Eq a, Num a) => (a -> a -> a) -> Vector a -> Vector a -> Vector a
composeVectorNum = composeVectorDefault 0

length :: Floating a => Vector a -> a
length v = sqrt $ getX v ** 2 + getY v ** 2 + getZNum v ** 2

sum :: (Num a, Eq a) => Vector a -> Vector a -> Vector a
sum = composeVectorNum (+)

scalar :: (Num a, Eq a) => Vector a -> Vector a -> Vector a
scalar = composeVectorNum (*)

dist :: (Num a, Eq a) => Vector a -> Vector a -> Vector a
dist = composeVectorNum (-)

prod :: (Num a, Eq a) => Vector a -> Vector a -> Vector a
prod l r = toVectorNum (getY l * getZNum r - getZNum l * getY r) (getZNum l * getX r - getX l * getZNum r) (getX l * getY r - getY l * getX r)
