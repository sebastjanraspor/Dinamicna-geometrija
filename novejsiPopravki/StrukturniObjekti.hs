module StrukturniObjekti where

import Circle
import Data.List (sort)


data OsnovniObjekt a = Tocka (a,a) | Kroznica (a,a) a | Premica (a,a) (a,a) deriving (Show, Eq, Ord, Read)
--iskaze se da je dovolj pameten, da tudi prebere sam
	
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

zaokrozi n x = (fromInteger $ round $ x * (10^n)) / (10.0^^n)
na4dm = zaokrozi 4
-- pomebno za vecje podatke bolj natancno za manjse manj
        
--iz racunsko v cloveku prijazno
tr_racunska_standrdna :: (Circle Known) -> OsnovniObjekt Double        
tr_racunska_standrdna (Circle p a b k)
    |(10^6)*abs(p) < 1 = Premica (na4dm 0, na4dm k) (na4dm x1, na4dm y1)
    | otherwise = objekt (na4dm x, na4dm y) r 
    where
	x1=100
	y1=(a/b)*100
	x = a / p
	y = b / p
	r = sqrt (x ^ 2 + y ^ 2 - k / p)
	objekt (x,y) r
	    | (10^6)* r < 1 =  Tocka (x,y)
		| otherwise = Kroznica (x,y) (na4dm r)            
        
        
--iz cloveku prijazne v racunsko
tr_standardna_racunska :: OsnovniObjekt Double -> Circle Known
tr_standardna_racunska (Tocka (x,y)) = tr0_kr_stevila_racunska x y 0
tr_standardna_racunska (Kroznica (x,y) r) = tr0_kr_stevila_racunska x y r
tr_standardna_racunska (Premica (x0,y0) (x1,y1)) = tr_pr_stevila_racunska x0 y0 x1 y1

---------------------------------------------------------------------------------
---------------------------------------------------------------------------------
---------------------------------------------------------------------------------

vrniOrentaciji (Premica (x1,y1) (x2,y2)) = [line x1 y1 x2 y2, line x2 y2 x1 y1]
vrniOrentaciji (Kroznica (x,y) r) = [clockwise_circle x y r, anticlockwise_circle x y r]
-- vrniOrentaciji (Tocka (x,y)) = [clockwise_circle x y 0, anticlockwise_circle x y 0]
-- ta nacin ne da rezultatov !!!

-- poisciVseMozneOrentacije sez = rrr
    -- where
        -- [a,b,c] = map vrniOrentaciji sez --ce vec pogojev treba drugace
        -- rrr = [[k1,k2,k3]|k1 <- a, k2 <- b, k3 <- c]
        
vrniVseKrozniceKiZadoscajoP k0 pogojiSez = map (fffKrZapis k0) (poisciVseMozneKombinacije pogojiSez)
    where
        fffKrZapis k0 x = find k0 $ satisfying x
        poisciVseMozneKombinacije [a,b,c] = [[k1,k2,k3]|k1 <- a, k2 <- b, k3 <- c]

vrniVseKrozniceKiZadoscajo k0 sezPogoji = rrr
    where
        [c01,c02] = vrniOrentaciji k0
        rrr01 = vrniVseKrozniceKiZadoscajoP c01 sezPogoji
        rrr02 = vrniVseKrozniceKiZadoscajoP c02 sezPogoji
        rrr00 = map tr_racunska_standrdna (odstraniMaybe (rrr01 ++ rrr02))
        rrr = sortirajIzlociPodvojene rrr00
        
sortirajIzlociPodvojene sez = novSez
    where
        sezSort = sort sez
        novSez = odstraniDvojnike sezSort
 
odstraniDvojnike [] = []
odstraniDvojnike [b] = [b]
odstraniDvojnike (a:b:sezSort)
    |a==b = odstraniDvojnike (b:sezSort)
    |otherwise = a:(odstraniDvojnike (b:sezSort))
        
odstraniMaybe [] = []
odstraniMaybe (Nothing:sez) = odstraniMaybe sez
odstraniMaybe ((Just x):sez) = x:(odstraniMaybe sez)
        
-- dolociElement (Just x1) _ = Just x1
-- dolociElement Nothing a = a -- a je Just nekaj ali Nothing
-- posebnoZdruzenjeSeznamov [] _ = []
-- posebnoZdruzenjeSeznamov _ [] = [] --sicer naceloma to ni potrebno saj morata biti enako dolga
-- posebnoZdruzenjeSeznamov (x1:sez1) (x2:sez2) = (dolociElement x1 x2):posebnoZdruzenjeSeznamov sez1 sez2





wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
split c = wordsWhen (==c)

branjePodatka "" = ""
branjePodatka ('[':ostanek) = branjePodatka2 ostanek
branjePodatka (x:ostanek) = branjePodatka ostanek

branjePodatka2 "" = ""
branjePodatka2 (']':ostanek) ='$':(branjePodatka ostanek)
branjePodatka2 (x:ostanek) = x:(branjePodatka2 ostanek)

branjePodatkov besedilo = split '$' (branjePodatka besedilo)
razdelitevPodatkov besedilo = map (split ';') (branjePodatkov besedilo)


poisciUstrezneKroge :: [Char] -> [Char] -> (OsnovniObjekt Double) -> [OsnovniObjekt Double]
poisciUstrezneKroge nizOmejitev besedilo osObjekt = vrniVseKrozniceKiZadoscajo osObjekt sezPogoji
    where
        [a,b,c] = preberiNizOmejitev nizOmejitev
        sezPogoji = map narediSezMoznihOmejitevDvojice (pripniPogojeOdstraniOstale a b c (razdelitevPodatkov besedilo))

preberiNizOmejitev :: [Char] -> [([Char], [Char])]
preberiNizOmejitev nizOmejitev = map preberiDvojico (split ';' nizOmejitev)
    where
        preberiDvojico niz = (a,b)
            where
                brezPrvegaInZadnjega (x:niz)= brezZadnjega niz
                brezZadnjega "" = ""
                brezZadnjega (x:"") = ""
                brezZadnjega (x:niz) = x:(brezZadnjega niz)
                [a,b] = split ',' (brezPrvegaInZadnjega niz)

pripniPogojeOdstraniOstale a b c sez = odstraniMaybe (map (najdiPretvori a b c) sez)

najdiPretvori (a,a1) (b,b1) (c,c1) (glava:osObjekt:_)
    |a==glava = Just (((read osObjekt) :: OsnovniObjekt Double),a1)
    |b==glava = Just (((read osObjekt) :: OsnovniObjekt Double),b1)
    |c==glava = Just (((read osObjekt) :: OsnovniObjekt Double),c1)
    |otherwise = Nothing
            
narediSezMoznihOmejitevDvojice ((Tocka (x,y)), pogoj) = [omejitev]
    where
        omejitevTocke (x,y) "s" = passing_through x y
        omejitevTocke (x,y) "c" = centred_at x y
        omejitev = omejitevTocke (x,y) pogoj
            
narediSezMoznihOmejitevDvojice (objekt, pogoj) = map (narediOmejitev pogoj) (vrniOrentaciji objekt)
    where
        narediOmejitev :: [Char] -> (Circle Known -> Circle Unknown -> Results)
        narediOmejitev "t" = tangent_to
        narediOmejitev ('x':kot) = crossing_angle ((read kot)::Double)
        narediOmejitev "v" = concentric_with
