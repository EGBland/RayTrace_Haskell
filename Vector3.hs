module Vector3 (
    Vec3,
    vecfoldl, vecfoldr,
    vecmap, vecop,
    (£+), (£-), (£*), (£.), (£^),
    vecnorm, vecunit,
    vx, vy, vz
) where

type Vec3 a = (a,a,a)

vecfoldl :: (b -> a -> b) -> b -> Vec3 a -> b
vecfoldl f i (x,y,z) = ((i `f` x) `f` y) `f` z

vecfoldr :: (a -> b -> b) -> b -> Vec3 a -> b
vecfoldr f i (x,y,z) = x `f` (y `f` (z `f` i))

vecmap :: (a -> b) -> Vec3 a -> Vec3 b
vecmap f (x,y,z) = (f x,f y,f z)

vecop :: (a -> b -> c) -> Vec3 a -> Vec3 b -> Vec3 c
vecop f (x1,y1,z1) (x2,y2,z2) = (f x1 x2,f y1 y2,f z1 z2)

(£+) :: (Num a) => Vec3 a -> Vec3 a -> Vec3 a
(£+) = vecop (+)

(£-) :: (Num a) => Vec3 a -> Vec3 a -> Vec3 a
(£-) = vecop (-)

(£*) :: (Num a) => a -> Vec3 a -> Vec3 a
(£*) t = vecmap (*t)

(£.) :: (Num a) => Vec3 a -> Vec3 a -> a
(x1,y1,z1) £. (x2,y2,z2) = x1*x2 + y1*y2 + z1*z2

(£^) :: (Num a) => Vec3 a -> Vec3 a -> Vec3 a
(x1,y1,z1) £^ (x2,y2,z2) = (y1*z2-y2*z1,x2*z1-x1*z2,x1*y2-x2*y1)    

vecnorm :: (Floating a) => Vec3 a -> a
vecnorm (x,y,z) = sqrt (x*x + y*y + z*z)

vecunit :: (Floating a) => Vec3 a -> Vec3 a
vecunit v = (1/vecnorm v) £* v

vx :: Vec3 a -> a
vx (x,_,_) = x

vy :: Vec3 a -> a
vy (_,y,_) = y

vz :: Vec3 a -> a
vz (_,_,z) = z