module PretvorbePodatkov where

import HTk.Toplevel.HTk
import StrukturniObjekti


osnovniPretvornik1 :: Distance -> Float
osnovniPretvornik1 a = read(show(a))

osnovniPretvornik2 ::  Float -> Distance
osnovniPretvornik2 a = read(show(a))

-- osnovniPretvornik3 :: Distance -> Double
-- osnovniPretvornik3 a = read(show(a))

osnovniPretvornik4 :: Double -> Distance
osnovniPretvornik4 a = read(show(a))

pretvoriIzStandardneOblike :: OsnovniObjekt Double -> (Distance,Distance,Distance,Distance)
pretvoriIzStandardneOblike (Kroznica (x,y) r) =(x0a,y0a,x1a,y1a)
    where
        x0fl=x-r
        y0fl=y-r
        x1fl=x+r
        y1fl=y+r
        x0a=(osnovniPretvornik4 x0fl) ::Distance
        y0a=(osnovniPretvornik4 y0fl) ::Distance
        x1a=(osnovniPretvornik4 x1fl) ::Distance
        y1a=(osnovniPretvornik4 y1fl) ::Distance
        

pretvorbaOmejenaRadialna x0 y0 x1 y1 = (xk1,xk2,yk1,yk2)
    where
        x0fl=osnovniPretvornik1 x0 ::Float
        y0fl=osnovniPretvornik1 y0 ::Float
        x1fl=osnovniPretvornik1 x1 ::Float
        y1fl=osnovniPretvornik1 y1 ::Float
        r=sqrt((x1fl-x0fl)^2+(y1fl-y0fl)^2)
        (xs,ys)=(x0fl,y0fl)
        xk1=osnovniPretvornik2 (xs-r)
        xk2=osnovniPretvornik2 (xs+r)
        yk1=osnovniPretvornik2 (ys-r)
        yk2=osnovniPretvornik2 (ys+r)

standardizirajObliko :: [Char] -> Distance -> Distance -> Distance -> Distance -> (OsnovniObjekt Double)
--standardizirajObliko nizObjekta x0 y0 x1 y1        
standardizirajObliko "Kroznica" x0 y0 x1 y1 = osObjekt
    where
        x0fl=(osnovniPretvornik1 x0) ::Float
        y0fl=(osnovniPretvornik1 y0) ::Float
        x1fl=(osnovniPretvornik1 x1) ::Float
        y1fl=(osnovniPretvornik1 y1) ::Float
        r=sqrt((x1fl-x0fl)^2+(y1fl-y0fl)^2)
        (xs,ys)=(x0fl,y0fl)

        niz = " Kroznica "++show((xs,ys))++" "++show(r)
        osObjekt = (read niz):: (OsnovniObjekt Double)
        -- Tole zgoraj je NAPAKA HASKLLA ce namesto niz damo kar vsebuje ne dela!!!!!
        
standardizirajObliko "Premica" x0 y0 x1 y1 = (read niz):: (OsnovniObjekt Double)
    where
        x0fl=(osnovniPretvornik1 x0) ::Float
        y0fl=(osnovniPretvornik1 y0) ::Float
        x1fl=(osnovniPretvornik1 x1) ::Float
        y1fl=(osnovniPretvornik1 y1) ::Float
        niz = " Premica "++show((x0fl,y0fl))++" "++show((x1fl,y1fl))
        
standardizirajObliko "Tocka" x0 y0 _ _ = (read niz):: (OsnovniObjekt Double)
    where
        x0fl=(osnovniPretvornik1 x0) ::Float
        y0fl=(osnovniPretvornik1 y0) ::Float
        niz = " Tocka "++show((x0fl,y0fl))
        
        
-- super ugotovitev: Kasnejsega parsa ne motijo presledki derive (Read)
sestaviBesediloElementa :: [Char] -> Distance -> Distance -> Distance -> Distance -> Distance -> [Char]
sestaviBesediloElementa "Kroznica" x0 y0 x1 y1 zaporednaSt = razsirjeniNizObKor
    where
        x0fl=(osnovniPretvornik1 x0) ::Float
        y0fl=(osnovniPretvornik1 y0) ::Float
        x1fl=(osnovniPretvornik1 x1) ::Float
        y1fl=(osnovniPretvornik1 y1) ::Float
        r=sqrt((x1fl-x0fl)^2+(y1fl-y0fl)^2)
        (xs,ys)=(x0fl,y0fl)
        razsirjeniNizObKor = "[K"++show(zaporednaSt)++"; Kroznica "++show((xs,ys))++" "++show(r)++"];//;"
        
sestaviBesediloElementa "Premica" x0 y0 x1 y1 zaporednaSt = razsirjeniNizObKor
    where
        x0fl=osnovniPretvornik1 x0 ::Float
        y0fl=osnovniPretvornik1 y0 ::Float
        x1fl=osnovniPretvornik1 x1 ::Float
        y1fl=osnovniPretvornik1 y1 ::Float
        razsirjeniNizObKor = "[P"++show(zaporednaSt)++"; Premica "++show((x0fl,y0fl))++" "++show((x1fl,y1fl))++"];//;"
        
sestaviBesediloElementa "Daljica" x0 y0 x1 y1 zaporednaSt = razsirjeniNizObKor
    where
        x0fl=osnovniPretvornik1 x0 ::Float
        y0fl=osnovniPretvornik1 y0 ::Float
        x1fl=osnovniPretvornik1 x1 ::Float
        y1fl=osnovniPretvornik1 y1 ::Float
        razsirjeniNizObKor = "[D"++show(zaporednaSt)++"; Daljica "++show((x0fl,y0fl))++" "++show((x1fl,y1fl))++"];//;"

sestaviBesediloElementa "Tocka" x0 y0 _ _ zaporednaSt = razsirjeniNizObKor
    where
        x0fl=osnovniPretvornik1 x0 ::Float
        y0fl=osnovniPretvornik1 y0 ::Float
        razsirjeniNizObKor = "[T"++show(zaporednaSt)++"; Tocka "++show((x0fl,y0fl))++"];//;"
        
        

spremembaObjektaP :: [Char] -> Distance -> Distance -> Distance -> Distance -> [(Distance,Distance)]
spremembaObjektaP "Kroznica" x0 y0 x1 y1 = [(xk1,yk1),(xk2,yk2)]
    where
        (xk1,xk2,yk1,yk2) = pretvorbaOmejenaRadialna x0 y0 x1 y1

spremembaObjektaP "Daljica" x0 y0 x1 y1 = [(x0,y0),(x1,y1)]
spremembaObjektaP "Premica" x0 y0 x1 y1 = [(xk1,yk1),(xk2,yk2)]
    where
        x0fl=osnovniPretvornik1 x0 ::Float
        y0fl=osnovniPretvornik1 y0 ::Float
        x1fl=osnovniPretvornik1 x1 ::Float
        y1fl=osnovniPretvornik1 y1 ::Float
        sprX=x0fl-x1fl
        sprY=y0fl-y1fl
        k=sprY/sprX
        sirinaPlatna=1000
        xk1=osnovniPretvornik2 (x0fl-sirinaPlatna)
        xk2=osnovniPretvornik2 (x0fl+sirinaPlatna)
        yk1=osnovniPretvornik2 (y0fl-k*sirinaPlatna)
        yk2=osnovniPretvornik2 (y0fl+k*sirinaPlatna)

spremembaObjekta :: (CanvasItem b) =>[Char] -> Distance -> Distance -> Distance -> Distance -> Config b
spremembaObjekta niz x0 y0 x1 y1 =coord (spremembaObjektaP niz x0 y0 x1 y1)
