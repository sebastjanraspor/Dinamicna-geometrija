
Inversion.lhs
Calculates the inverse of a matrix.

> module Inversion (Result (..), newton_raphson) where




----- Results -----
A value of type Result consists of the result of evaluating a function on circles,
together with all of the function's derivatives at that point.

> data Result = R {value, dp, da, db :: Double} deriving (Read, Show)

> result :: Double -> ((Result -> Double) -> Double) -> Result
> result value derivative =
>   R value (derivative dp) (derivative da) (derivative db)

> constant :: Double -> Result
> constant x = R x 0 0 0 

> instance Eq Result
>   where
>   x == y = value x == value y
>   x /= y = value x /= value y

> instance Ord Result
>   where
>   compare x y = compare (value x) (value y)
>   x < y = value x < value y
>   x > y = value x > value y
>   x <= y = value x <= value y
>   x >= y = value x >= value y

> instance Num Result
>   where
>   x + y = result (value x + value y) (\d -> d x + d y)
>   x - y = result (value x - value y) (\d -> d x - d y)
>   x * y = result (value x * value y) (\d -> value x * d y + value y * d x)
>   negate x = result (negate (value x)) (\d -> negate (d x))
>   fromInteger x = constant (fromInteger x)
>   abs x
>     | value x > 0 = x
>     | value x < 0 = -x
>     | otherwise = result 0 (const (error "abs: not differentiable at 0"))
>   signum x
>     | value x > 0 = 1
>     | value x < 0 = -1
>     | otherwise = result 0 (const (error "signum: not differentiable at 0"))

> instance Fractional Result
>   where
>   x / y = result (value x / value y)
>     (\d -> (value y * d x - value x * d y) / (value y ^ 2))
>   fromRational x = constant (fromRational x)

> instance Floating Result
>   where
>   pi = constant pi
>   sqrt x = result (sqrt (value x)) (\d -> d x / 2 / sqrt (value x))
>   exp x = result (exp (value x)) (\d -> d x * exp (value x))
>   log x = result (log (value x)) (\d -> d x / value x)
>   x ** y = result (value x ** value y)
>     (\d -> value x ** (value y - 1) * value y * d x +
>     value x ** value y * log (value x) * d y)
>   x `logBase` y = result (value x `logBase` value y)
>     (\d -> (value x * log (value x) * d y - value y * log (value y) * d x) / 
>     (value x * value y * log (value x) ^ 2))
>   sin x = result (sin (value x)) (\d -> d x * cos (value x))
>   cos x = result (cos (value x)) (\d -> -d x * sin (value x))
>   tan x = result (tan (value x)) (\d -> d x / cos (value x) ^ 2)
>   asin x = result (asin (value x)) (\d -> d x / sqrt (1 - value x ^ 2))
>   acos x = result (acos (value x)) (\d -> -d x / sqrt (1 - value x ^ 2))
>   atan x = result (atan (value x)) (\d -> d x / sqrt (1 + value x ^ 2))
>   sinh x = result (sinh (value x)) (\d -> d x * cosh (value x))
>   cosh x = result (cosh (value x)) (\d -> d x * sinh (value x))
>   tanh x = result (tanh (value x)) (\d -> d x / cosh (value x) ^ 2)
>   asinh x = result (asinh (value x)) (\d -> d x / sqrt (value x ^ 2 + 1))
>   acosh x = result (acosh (value x)) (\d -> d x / sqrt (value x ^ 2 - 1))
>   atanh x = result (atanh (value x)) (\d -> d x / (1 - value x ^ 2))




----- Newton-Raphson Iteration -----
This is the heart of the package's Newton-Raphson iterator. Given a
list of Results, we calculate the displacement required to improve
the estimate. At each stage, we ignore any contradictions in the list
of results in the hope that they will go away in the neighbourhood of
the solution.

If we are a long way from a solution to the Newton-Raphson iteration,
then it is possible that each step might move us further from the solution,
not closer to it. In the event that the new estimate is worse than the old
one, we successively shrink the size of the step from the old estimate to
the new one until we find an estimate that is at least as good as the old
one. See [1], section 9.7.1

In order to ensure termination, we apply at most 10 shrinking steps in each
iteration. The 10 shrinking steps should be sufficient to recover a near-
singular problem that is not quite singular enough to be caught by the QR
decomposition below. We perform up to 25 Newton-Raphson iterations to get a
rough solution. Once we have obtained this rough solution, we always perform
another 5 iterations to ensure accuracy.

To perform these steps, we construct an infinite data structure and then walk
over it. Because of Haskell's lazy semantics, this is a common programming
idiom. It enables us to separate the mechanics of performing the calculation
from the calculation itself.

We need to test to ensure that the iteration has converged correctly. To do
this, we check that small changes in the variables cause overwhelmingly large
changes to the result value, and that the deltas are all small compared to
the guess. We declare that the result has converged when the guesses have at
least 3 digits of accuracy and the results are all zero to three digits of
accuracy. Applying a further 5 iterations will always be sufficient to ensure
that we have reached the maximum accuracy available.

> data NewtonRaphson = NR {guess :: [Double], badness :: Result,
>   converged :: Bool, next :: [NewtonRaphson]}
> instance Eq NewtonRaphson
>   where
>   nr1 == nr2 = badness nr1 == badness nr2
> instance Ord NewtonRaphson
>   where
>   compare nr1 nr2 = compare (badness nr1) (badness nr2)

-- > newton_raphson :: [Double] -> ([Result] -> [Result]) -> [Double]
-- > newton_raphson first_guess constraints = guess solution
-- >   where
-- >   nr = create_newton_raphson first_guess (constraints . variables)
-- >   iterations = filter converged $ take 100 $ iterate nr_iteration nr
-- >   solution = (iterations ++ error "Circle: Failed to converge") !! 50
-- >   nr_iteration = minimum . take 10 . next
-- >   variables [p, a, b] = [R p 1 0 0, R a 0 1 0, R b 0 0 1]

> newton_raphson :: [Double] -> ([Double] -> [Result]) -> [Double]
> newton_raphson first_guess constraints = guess solution
>   where
>   nr = create_newton_raphson first_guess constraints
>   iterations = filter converged $ take 100 $ iterate nr_iteration nr
>   solution = (iterations ++ error "Circle: Failed to converge") !! 50
>   nr_iteration = minimum . take 10 . next
                                                 
> create_newton_raphson :: [Double] -> ([Double] -> [Result]) -> NewtonRaphson
> create_newton_raphson guess constraints = nr
>   where
>   nr = NR guess badness converged next
>   results = constraints guess
>   deltas = delta results
>   badness = sum $ map (^ 2) results
>   insignificant r = 1e-6 * norm [dp r, da r, db r] >= abs (value r)
>   converged = all insignificant results && norm deltas <= 1e-3 * norm guess
>   next = create_next nr deltas constraints

> create_next :: NewtonRaphson -> [Double] -> ([Double] -> [Result]) -> [NewtonRaphson]
> create_next nr deltas constraints =
>   nr' : if good_enough || converged nr' then [] else nrs
>   where
>   nr' = create_newton_raphson (zipWith (-) (guess nr) deltas) constraints
>   nrs = create_next nr (map (* scale) deltas) constraints
>   gradient = dot [dp (badness nr), da (badness nr), db (badness nr)] deltas
>   difference = value (abs(badness nr - badness nr'))
>   scale = max 0.1 (gradient / (2 * (gradient - difference)))
>   good_enough = difference >= 1e-3 * (abs gradient)

----- Pseudo-inversion -----
The function delta takes the list of results generated by the constraints
and returns the amount by which the values must change to converge on the
solution. For robustness, we use the Moore-Penrose pseudo-inverse. When
the Jacobian passed to delta is invertible, then the inverse and pseudo-
inverse coincide. At other times, the pseudo-inverse gives us a sensible
way to continue the iteration.

In [1], section 2.5, it is recommended to make a second pass over the
result to reduce any residual errors.

Given a k x 3 upper triangular matrix R of rank k and a k x 1 vector X,
calculate R^+X, where R^+ is the Moore-Penrose pseudo-inverse. R is given
as a list of its columns. To keep things simple, we don't attempt to use
recursion here, but just list the cases.

> delta :: [Result] -> [Double]
> delta results = zipWith (-) deltas errors
>   where
>   ass = map (flip map results) [dp, da, db]
>   bss = map (flip map results) [value, error]
>   xss = map (pseudoinvert ass') bss'
>   (ass', bss', ~[deltas, errors]) = qr (ass, bss, xss)
>   error r = dot [dp r, da r, db r] deltas - value r

> pseudoinvert :: [[Double]] -> [Double] -> [Double]
> pseudoinvert [[], [], []] [] = [0, 0, 0]
> pseudoinvert [[r11], [r12], [r13]] [x1] = [y1, y2, y3]
>   where
>   y3 = r13 * scale
>   y2 = r12 * scale
>   y1 = r11 * scale
>   scale = x1 / (r11 * r11 + r12 * r12 + r13 * r13)
> pseudoinvert [[r11], [r12, r22], [r13, r23]] [x1, x2] = [y1, y2, y3]
>   where
>   y3_numer = x2 * r23 * r11 ^ 2 - (x1 * r22 - x2 * r12) * (r12 * r23 - r13 * r22)
>   y3_denom = (r12 * r23 - r13 * r22) ^ 2 + (r11 * r23) ^ 2 + (r11 * r22) ^ 2
>   y3 = y3_numer / y3_denom
>   x2' = x2 - y3 * r23
>   y2 = x2' / r22
>   x1' = x1 - y3 * r13 - y2 * r12
>   y1 = x1' / r11
> pseudoinvert [[r11], [r12, r22], [r13, r23, r33]] [x1, x2, x3] = [y1, y2, y3]
>   where
>   y3 = x3 / r33
>   x2' = x2 - y3 * r23
>   y2 = x2' / r22
>   x1' = x1 - y3 * r13 - y2 * r12
>   y1 = x1' / r11




----- QR decomposition -----
Given a matrix equation AX = B that we wish to solve for X, the first step
is to triangularise A. We find an orthogonal matrix Q and a permutation
matrix P such that QAP is upper triangular with each row either being zero
or having a non-zero entry on the diagonal. All zero rows are collected
together in the bottom part of the matrix. This reduces the problem to one
of finding the unknown P^TX such that (QAP)(P^TX) = QB. X can readily be
recovered once P^TX is known.

We define a function, qr, to perform these transformations. Suppose that
qr (ass, bss, xss) = (ass', bss', xss'). If ass is a list of the columns
of A, bss is a list of the columns of B and xss is a list of the columns
of P^TX, then ass' is a list of the columns of QAP, bss' is a list of the
columns of QB and xss' is a list of the columns of X. In ass', we omit
the zeros below the diagonal.

Of course, we are going to require part of the result of qr (ass, bss, xss)
in order to calculate the value of xss, so it looks like we have a circular
dependency here. Thanks to Haskell's lazy evaluation, this is just an illusion.

Our function works by performing Householder transformations. In order to
find the best-conditioned column in the table to use for the next Householder
transformation, we use a variant of bubble sort to bubble the element with the
highest norm to the front of the list. However, we will also wish to perform
the corresponding unsort so that the variables come out in the right order.
We achieve this by calculating the permutation that the sort would apply,
simultaneously calculating the inverse permutation.

Our Householder transformations evaluate the head of each list as a special
case in order to avoid loss of significance issues.

Other auxilliary functions should be self-explanatory.

> qr :: ([[Double]], [[Double]], [[Double]]) -> ([[Double]], [[Double]], [[Double]])
> qr (ass, bss, xss)
>   | small n = (map (const []) ass, map (const []) bss, xss)
>   | otherwise = ([n] : heads ass' ass'', heads bss' bss'', map unsort (heads xss xss''))
>   where
>   (n, sort, unsort) = bubble (map norm ass)
>   h = householder n (head $ sort ass)
>   ass' = map h (tail $ sort ass)
>   bss' = map h bss
>   (ass'', bss'', xss'') = qr (map tail ass', map tail bss', map tail xss)
>   heads = zipWith (:) . map head
>   small x = abs x < 1e-5

> bubble :: [Double] -> (Double, [a] -> [a], [b] -> [b])
> bubble norms = foldr combine (0, id, id) norms
>   where
>   combine n1 (n2, sort, unsort)
>     | n1 < n2   = (n2, swap . keep sort, keep unsort . swap)
>     | otherwise = (n1, id, id)
>   swap (x1 : x2 : xs) = x2 : x1 : xs
>   keep f (x : xs) = x : f xs

> householder :: Double -> [Double] -> ([Double] -> [Double])
> householder n vector = if head vector <= 0 then h1 else h2
>   where
>   vs1 = head vector - n : tail vector
>   h1 xs = dot vector xs / n : zipWith (\v x -> x - 2 * v * k) (tail vs1) (tail xs)
>     where
>     k = dot vs1 xs / dot vs1 vs1
>   vs2 = head vector + n : tail vector
>   h2 xs = dot vector xs / n : zipWith (\v x -> 2 * v * k - x) (tail vs2) (tail xs)
>     where
>     k = dot vs2 xs / dot vs2 vs2

> dot :: [Double] -> [Double] -> Double
> dot xs ys = sum (zipWith (*) xs ys)

> norm :: [Double] -> Double
> norm xs = sqrt (dot xs xs)




----- Gaussian Elimination -----
For debugging purposes, we include a Gaussian eliminator.

> debug_delta :: [Result] -> [Double]
> debug_delta results = map (sum . zipWith (*) values) inverse
>   where
>   values = map value results
>   jacobian = map (\r -> [dp r, da r, db r]) results
>   [[x11, x12, x13], [x21, x22, x23], [x31, x32, x33]] = jacobian
>   cofactors =
>     [[x22 * x33 - x32 * x23, x32 * x13 - x12 * x33, x12 * x23 - x22 * x13],
>     [x31 * x23 - x21 * x33, x11 * x33 - x31 * x13, x21 * x13 - x11 * x23],
>     [x21 * x32 - x31 * x22, x31 * x12 - x11 * x32, x11 * x22 - x21 * x12]]
>   determinant
>     = x11 * (x22 * x33 - x32 * x23)
>     - x12 * (x21 * x33 - x31 * x23)
>     + x13 * (x21 * x32 - x31 * x22)
>   inverse = (map . map) (/ determinant) cofactors




----- References -----
[1] "Numerical Recipes", Third edition, 2007.
William H. Press, Saul A. Teukolsky, William T. Vetterling and Brian P. Flannery.
Cambridge Uniersity Press.
