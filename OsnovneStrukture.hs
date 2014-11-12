data Circle a = Circle {p, a, b, k::a} deriving (Show, Eq) 
type Known = Double
data OsnovniObjekt a = Tocka (a,a) | Kroznica (a,a) a | Premica (a,a) (a,a) deriving (Show, Eq) 
	
--tr je okrajsava za transformacija

--iz stevil v racunsko
tr0_kr_stevila_racunska :: Double -> Double -> Double -> Circle Known
tr0_kr_stevila_racunska x y r = Circle 1 x y (x ^ 2 + y ^ 2 - r ^ 2)

tr1_kr_stevila_racunska :: Double -> Double -> Double -> Circle Known
tr1_kr_stevila_racunska x y r = Circle (-1) x y (x ^ 2 + y ^ 2 - r ^ 2)

tr_pr_stevila_racunska :: Double -> Double -> Double -> Double -> Circle Known
tr_pr_stevila_racunska a b c d = Circle 0 (-c1/2) (-c2/2) n
	where
	r=sqrt((c-a)^2+(d-b)^2) --to mi ni vsec raje bi ^(1/2)
	c1=(d-b)/r
	c2=(c-a)/r
	n=b-(c1/c2)*a

--iz racunsko v cloveku prijazno
tr_racunska_standrdna :: (Circle Known) -> OsnovniObjekt Double
tr_racunska_standrdna (Circle 0 a b k) = Premica (0,k) (x,y)
	where
	x=100
	y=(a/b)*100
	
tr_racunska_standrdna (Circle p a b k) = objekt (x,y) r
	where
	x = a / p
	y = b / p
	r = sqrt (x ^ 2 + y ^ 2 - k / p)
	objekt (x,y) r
	    | r <= 0.001 =  Tocka (x,y)
		| otherwise  =  Kroznica (x,y) r

--iz cloveku prijazne v racunsko
tr_standardna_racunska :: OsnovniObjekt Double -> Circle Known
tr_standardna_racunska (Tocka (x,y)) = tr0_kr_stevila_racunska x y 0
tr_standardna_racunska (Kroznica (x,y) r) = tr0_kr_stevila_racunska x y r
tr_standardna_racunska (Premica (x0,y0) (x1,y1)) = tr_pr_stevila_racunska x0 y0 x1 y1

test1=(tr_racunska_standrdna(tr_standardna_racunska (Kroznica (1,1) 3)) == (Kroznica (1,1) 3))
test2=(tr_racunska_standrdna(tr_standardna_racunska (Tocka (2,3))) == Tocka (2,3))
--test3 "odpove ker v nazaj pac dolocimo ene tocke na premici jaz sem izbral kot predstavnika premice k in (100, 100*k)."
test3=(tr_racunska_standrdna(tr_standardna_racunska (Premica (5,8) (1,-1))) == Premica (5,8) (1,-1))
test4=(tr_racunska_standrdna(tr_standardna_racunska (Premica (1,3) (2,6))) == tr_racunska_standrdna(tr_standardna_racunska (Premica (3,9) (-2,-6))))
