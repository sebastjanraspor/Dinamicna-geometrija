Circle.lhs
Performs various calculations on circles.

> {-# OPTIONS -XFlexibleInstances #-}
> module Circle where
> import Inversion




------ Data type of circles ------

> data Circle a = Circle {p, a, b, k :: a}
> type Known = Double
> type Unknown = Result

> class (Floating a, Ord a) => CircleType a
>   where
>   coerce_constant :: Double -> a
>   coerce_circle :: Circle Known -> Circle a

> instance CircleType Double
>   where
>   coerce_constant x = x
>   coerce_circle c = c

> instance CircleType Result
>   where
>   coerce_constant x = R x 0 0 0
>   coerce_circle (Circle p a b k) =
>     Circle (R p 0 0 0) (R a 0 0 0) (R b 0 0 0) (R k 0 0 0)


----- Human-friendly descriptions of circles -----
Convert circles to a format that is easier to read and understand

> circle :: Double -> Double -> Double -> Circle Known
> circle x y r = Circle 1 x y (x ^ 2 + y ^ 2 - r ^ 2)

> line :: Double -> Double -> Double -> Double -> Circle Known
> line x1 y1 x2 y2
>     | x1 /= x2 = Circle 0 c 1 d
>     | otherwise = Circle 1 1 0 (-x1)
>     where
>     c = (y2-y1)/(x1-x2)
>     d = -c*x1 - y1

Lines are shown in a format "line x y". The values coordintates x and y
are the intersections of the line with x and y axis, respectively.
      


> instance Show (Circle Known)
>   where
>   show c@(Circle p a b k)
>     | {is_line = unwords ["line", show x, show y]
>       where (x, y) = axes_intersection c}
>     | otherwise   = unwords ["circle", show x, show y, show r]
>     where
>     is_line = a ^ 2 + b ^ 2 - k * p >= 1e18 * p ^ 2
>     x = a / p
>     y = b / p
>     r = sqrt (x ^ 2 + y ^ 2 - k / p)


-- >     d = k / 2 / sqrt (a ^ 2 + b ^ 2)
-- >     theta = atan2 (-a) b
-- >     show x = showParen (x < 0) (shows x) ""

-- > instance Read (Circle Known)
-- >   where
-- >   readsPrec _ = readParen False $ \s1 ->
-- >     [(anticlockwise_circle x y r, s5) |
-- >       ("anticlockwise_circle", s2) <- lex s1,
-- >       (x, s3) <- reads s2,
-- >       (y, s4) <- reads s3,
-- >       (r, s5) <- reads s4] ++
-- >     [(clockwise_circle x y r, s5) |
-- >       ("clockwise_circle", s2) <- lex s1,
-- >       (x, s3) <- reads s2,
-- >       (y, s4) <- reads s3,
-- >       (r, s5) <- reads s4] -- ++

-- >     [(line d theta, s4) |
-- >       ("line", s2) <- lex s1,
-- >       (d, s3) <- reads s2,
-- >       (theta, s4) <- reads s3]




----- Find circles satisfying constraints -----

> newtype Results = Rs {unRs :: [Result]}

> satisfying :: [Circle Unknown -> Results] -> Circle Unknown -> Results
> satisfying constraints = concatRs . getRs
>   where
>   concatRs = Rs . concat . map unRs
>   getRs = flip map constraints . flip ($)

> find :: Circle Known -> (Circle Unknown -> Results) -> Circle Known
> find c@(Circle p a b k) constraints =
>   let variables [p, a, b] = [R p 1 0 0, R a 0 1 0, R b 0 0 1] in
>   circ c $ newton_raphson [p, a, b] (unRs . constraints . (circ c) . variables)

--> find_all :: Double -> Double -> Double -> (Circle Unknown -> Results) -> Circle Known
--> find_all a b acc constraints =

> circ :: CircleType a => Circle Known -> [a] -> Circle a
> circ c [p', a', b'] = Circle p' a' b' k'
>   where
>   Circle p a b k = coerce_circle c
>   x = (p' - p) * (p' + p) + (a' - a) * (a' + a) + (b' - b) * (b' + b)
>   k'
>     | k <  0 = k + x
>     | k >= 0 = k - x

> valid :: Circle Known -> Bool
> valid (Circle p a b k)
>   | any isNaN params      = False --error "Circle: NaN"
>   | any isInfinite params = False --error "Circle: Infinity"
>   | imaginary             = False --error "Circle: Imaginary"
>   | otherwise             = True
>   where
>   params = [p, a, b, k]
>   imaginary = a ^ 2 + b ^ 2 - k * p < 0


----- Intersections

> pr :: Double ->  Integer -> Result
> pr x 1 = R x 1 0 0
> pr y 2 = R y 0 1 0
> pr z 3 = R z 0 0 1

> circle_function :: Double -> Double -> Double -> Circle Double -> Result
> circle_function x y z (Circle p a b k) = val
>   where
>   xx = pr x 1
>   yy = pr y 2
>   pp = coerce_constant p
>   aa = coerce_constant (2*a)
>   bb = coerce_constant (2*b)
>   kk = coerce_constant k
>   val = pp*(xx^2 + yy^2) - aa*xx - bb*yy + kk

> intersection :: Double -> Double -> Circle Known -> Circle Known -> (Double, Double)
> intersection x0 y0 c1 c2 = (result !! 0, result !! 1)
>   where
>   f [x, y, z] = [circle_function x y z c1, circle_function x y z c2, R 0 0 0 1]
>   result = newton_raphson [x0, y0, 0] f

> axes_intersection :: Circle Known -> (Double, Double)
> axes_intersection (Circle p a b k) = (x', y')
>   where
>   x = k/(2*a)
>   y = k/(2*b)
>   x' | th1 <= 1e-18 = 1/0
>      | otherwise = x
>      where th1 = abs ((atan2 x y) - pi/2) + abs ((atan2 x y) + pi/2
>   y' | th2 <= 1e-18 = 1/0
>      | otherwise = y
>      where th2 = abs ((atan2 y x) - pi/2) + abs ((atan2 y x) + pi/2

Various useful ways to transform circles.

> reverse_circle :: CircleType a => Circle a -> Circle a
> reverse_circle (Circle p a b k) = Circle (-p) (-a) (-b) (-k)

> offset_circle :: CircleType a => Double -> Circle a -> Circle a
> offset_circle amount (Circle p a b k) = Circle p a b k'
>   where
>   amount' = coerce_constant amount
>   k' = k - 2 * amount' * sqrt (a ^ 2 + b ^ 2 - k * p) - p * amount' ^ 2

> translate_circle :: CircleType a => Double -> Double -> Circle a -> Circle a
> translate_circle x y (Circle p a b k) = Circle p a' b' k'
>   where
>   x' = coerce_constant x
>   y' = coerce_constant y
>   a' = a + p * x'
>   b' = b + p * y'
>   k' = p * (x' ^ 2 + y' ^ 2) + 2 * a * x' + 2 * b * y' + k

> rotate_circle :: CircleType a => Double -> Circle a -> Circle a
> rotate_circle theta (Circle p a b k) = Circle p a' b' k
>   where
>   st = coerce_constant (sin theta)
>   ct = coerce_constant (cos theta)
>   a' = a * ct - b * st
>   b' = a * st + b * ct

> scale_circle :: CircleType a => Double -> Circle a -> Circle a
> scale_circle factor (Circle p a b k) = Circle p a' b' k'
>   where
>   factor' = coerce_constant factor
>   a' = factor' * a
>   b' = factor' * b
>   k' = factor' ^ 2 * k




----- Some constraints -----

> passing_through :: Double -> Double -> Circle Unknown -> Results
> passing_through x y (Circle p a b k) = Rs [value]
>   where
>   x' = coerce_constant x
>   y' = coerce_constant y
>   value = p * (x' ^ 2 + y' ^ 2) - 2 * a * x' - 2 * b * y' + k

> crossing_angle :: Double -> Circle Known -> Circle Unknown -> Results
> crossing_angle theta c1 c2 = Rs [value]
>   where
>   c = coerce_constant (cos theta)
>   Circle p1 a1 b1 k1 = coerce_circle c1
>   Circle p2 a2 b2 k2 = c2
>   --sqrt1 = sqrt (a1 ^ 2 + b1 ^ 2 - k1 * p1)
>   --sqrt2 = sqrt (a2 ^ 2 + b2 ^ 2 - k2 * p2)
>   --value = k1 * p2 + k2 * p1 + 2 * sqrt1 * sqrt2 * c - 2 * a1 * a2 - 2 * b1 * b2
>   val1 = a1 ^ 2 + b1 ^ 2 - k1 * p1
>   val2 = a2 ^ 2 + b2 ^ 2 - k2 * p2
>   val3 = k1 * p2 + k2 * p1 - 2 * a1 * a2 - 2 * b1 * b2
>   value = (2*c*val1*val2)^2 - val3^2

> tangent_to :: Circle Known -> Circle Unknown -> Results
> tangent_to = crossing_angle 0

> anticlockwise_radius :: Double -> Circle Unknown -> Results
> anticlockwise_radius r (Circle p a b k) = Rs [value]
>   where
>   r' = coerce_constant r
>   value = r' * p - sqrt (a ^ 2 + b ^ 2 - k * p)

> clockwise_radius :: Double -> Circle Unknown -> Results
> clockwise_radius r (Circle p a b k) = Rs [value]
>   where
>   r' = coerce_constant r
>   value = r' * p + sqrt (a ^ 2 + b ^ 2 - k * p)

> is_line :: Circle Unknown -> Results
> is_line (Circle p a b k) = Rs [p]

> concentric_with :: Circle Known -> Circle Unknown -> Results
> concentric_with c1 c2 = Rs [value1, value2, value3]
>   where
>   Circle p1 a1 b1 k1 = coerce_circle c1
>   Circle p2 a2 b2 k2 = c2
>   value1 = p2 * (a1 ^ 2 + b1 ^ 2) - p1 * (a1 * a2 + b1 * b2)
>   value2 = a2 * (p1 ^ 2 + b1 ^ 2) - a1 * (p1 * p2 + b1 * b2)
>   value3 = b2 * (p1 ^ 2 + a1 ^ 2) - b1 * (p1 * p2 + a1 * a2)

> centred_at :: Double -> Double -> Circle Unknown -> Results
> centred_at x y = concentric_with (Circle 1 x y 0)
