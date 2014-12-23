> module Main where
> import Circle

> data Point = P Double Double

> add :: Point -> Point -> Point
> add (P x1 y1) (P x2 y2) = P (x1+x2) (y1+y2)
>
> lmul :: Double -> Point -> Point
> lmul a (P x y) = P (a*x) (a*y)
>
> midpoint :: Point -> Point -> Point
> midpoint p q = lmul 0.5 (add p q)
>                  
> dist :: Point -> Point -> Double
> dist (P x1 y1) (P x2 y2) = sqrt ((x2-x1)^2 + (y2-x1)^2)
>
> line_through :: Point -> Point -> Circle Known
> line_through (P x1 y1) (P x2 y2) = line x1 y1 x2 y2

> data Triangle = T Point Point Point

> barycenter :: Triangle -> Point
> barycenter (T p1 p2 p3) = lmul (1/3) (add p1 (add p2 p3))

> incircle :: Triangle -> Circle Known
> incircle t@(T p1 p2 p3) = find (circle x0 y0 r) $ satisfying
>                       (map tangent_to [l1, l2, l3])
>     where
>     l1 = line_through p1 p2
>     l2 = line_through p2 p3
>     l3 = line_through p3 p1
>     p@(P x0 y0) = barycenter t
>     r = dist p p1  
       
----- Inscribed circle
      
> t = T (P (-1) 0) (P 0 1) (P 1 0)
> c = incircle t
