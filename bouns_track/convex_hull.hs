-- From exercises of Real World Haskell Chapter 03
-- Calulate the convex hull of a set of points
--
-- 10. Consider three two-dimensional points, a, b, and c. If we look at the
-- angle formed
-- by the line segment from a to b and the line segment from b to c, it turns
-- left, turns
-- right, or forms a straight line. Define a Direction data type that lets you
-- represent
-- these possibilities.
--
-- 11. Write a function that calculates the turn made by three two-dimensional
-- points
-- and returns a Direction .
--
-- 12. Define a function that takes a list of two-dimensional points and
-- computes the
-- direction of each successive triple. Given a list of points [a,b,c,d,e] , it
-- should
-- begin by computing the turn made by [a,b,c] , then the turn made by [b,c,d] ,
-- then [c,d,e] . Your function should return a list of Direction .
--
-- 13. Using the code from the preceding three exercises, implement Graham’s
-- scan al-
-- gorithm for the convex hull of a set of 2D points. You can find good
-- description
-- of what a convex hull (http://en.wikipedia.org/wiki/Convex_hull) is, and how
-- the
-- Graham scan algorithm (http://en.wikipedia.org/wiki/Graham_scan) should work,
-- on Wikipedia (http://en.wikipedia.org/).

data Point = Point {
   x :: Float,
   y :: Float } deriving Show

data Direction = LeftTurn | RightTurn | StraightTurn deriving Show

vector :: Point -> Point -> [Float]
vector f t = [x t - x f, y t - y f]

vector_mod :: [Float] -> Float
vector_mod v = sqrt (sum (map (^2) v))

vector_dot :: [Float] -> [Float] -> Float
vector_dot v w = sum (zipWith (*) v w)

vector_versor :: [Float] -> [Float]
vector_versor v = map (/ (vector_mod v)) v

vector_normal:: [Float] -> [Float]
vector_normal (x:y:[]) = [-y, x]

vector_rebase :: [Float] -> [Float] -> [Float]
vector_rebase b v = [vector_dot v base_x, vector_dot v base_y]
   where base_x = vector_versor b
         base_y = vector_normal base_x

turn_direction :: Point -> Point -> Point -> Direction
turn_direction a b c
   | (rebased !! 1) > 0 = LeftTurn
   | (rebased !! 1) < 0 = RightTurn
   | otherwise = StraightTurn
   where vector_ab = vector a b
         vector_bc = vector b c
         rebased = vector_rebase vector_ab vector_bc

turns :: [Point] -> [Direction]
truns [] = error "Please provide at least three points."
turns [_] = error "Please provide at least three points."
turns [_,_] = error "Please provide at least three points."
turns [a,b,c] = [turn_direction a b c]
turns (a:b:c:points) = [turn_direction a b c] ++ (turns (b:c:points))

my_sort_by :: (a -> a -> Ordering) -> [a] -> [a]
my_sort_by cmp [] = []
my_sort_by cmp (p:ps) = this_sort ([q | q <- ps , cmp q p == LT]) ++ [p] ++ this_sort([q | q <- ps , cmp q p /= LT])
   where this_sort = my_sort_by cmp

sort_for_graham :: [Point] -> [Point]
sort_for_graham ps = my_sort_by cmp ps
   where cmp p q = case cmpy of
                     EQ -> compare (x p) (x q)
                     LT -> cmpy 
                     GT -> cmpy 
                  where cmpy = compare (y p) (y q)

sort_by_angle_with :: Point -> [Point] -> [Point]
sort_by_angle_with p qs = my_sort_by cmp qs
      where cosine q = vector_versor(vector p q) !! 0
            cmp q qq = compare (cosine qq) (cosine q)

--d = [(Point 0 0), (Point 1 0.5), (Point 0.75 1), (Point 0.25 0.5), (Point (-0.25) 0.75), (Point (-0.5) 0.5)]
--d = [(Point 0 0), (Point 1 0.5), (Point 0.75 1), (Point 0.25 0.5), (Point (-0.25) 0.75)]
d = [(Point 0 0), (Point 1 0), (Point 1 1), (Point 0 1), (Point 0.5 0.5)]

convex_hull :: [Point] -> [Point]
convex_hull [] = error "Provide at least tree points"
convex_hull (y:[]) = error "Provide at least tree points"
convex_hull (x:y:[]) = error "Provide at least tree points"
convex_hull (p:ps) = helper (sorted_by_angle )
   where sorted_by_y = sort_for_graham (p:ps) --you just need the point at the bottom
         sorted_by_angle = [head sorted_by_y] ++ (sort_by_angle_with (head sorted_by_y) (tail sorted_by_y))
         helper [a,b,c] =  case (turn_direction a b c) of
                              LeftTurn -> [a,b,c]
                              RightTurn -> [a,c]
                              StraightTurn -> [a,c]
         helper (a:b:c:ps) =  case (turn_direction a b c) of
                                 LeftTurn -> a:helper(b:c:ps) 
                                 RightTurn -> helper(a:c:ps)
                                 StraightTurn -> helper(a:c:ps)
         help [a,b] = error "Two point left"
         help [a] = error "One point left"
         help [] = []


-- vim: expandtab
