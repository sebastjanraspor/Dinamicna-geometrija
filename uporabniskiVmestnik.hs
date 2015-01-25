import HTk.Toplevel.HTk
--import System.Random
import HTk.Toolkit.MarkupText as MarkupText

import Data.IORef
import System.IO.Unsafe

import Examples
import Circle

import OsnovneStrukture --(OsnovniObjekt,tr_racunska_standrdna)

--data OsnovniObjekt a = Tocka (a,a) | Kroznica (a,a) a | Premica (a,a) (a,a) deriving (Show, Eq)
-- zgoraj namesto premice daljica


-- data Osnova = CanvasItem Line | CanvasItem Oval

-- globalizirajSpremenljivko :: a -> IORef a
-- globalizirajSpremenljivko a = unsafePerformIO(newIORef a)

-- lokalizirajSpremenljivko :: IORef a -> a
-- lokalizirajSpremenljivko = unsafePerformIO . readIORef 

-- fffnizIOStevilo elIOniz = read(show(elIOniz))::Int
-- fffnizIODistance elIOniz = read(show(fffnizIOStevilo elIOniz))::Distance

--data Omejitev a = ()

--data OmejitvePodatki a = [,(),()]


--preberiPodatke ('K':st:_:preostanek) = maliParser preostanek--show (map preberiOmejitve (read(preostanek)))


--preberiOmejitve (niz1,niz2) = niz1++"test"++niz2

-- import Examples


--branjeOmejitve ('K':st:'t') = 

-- branjeOmejitve ('T':st:'s') = passing_through x1 x2
-- where
    -- (x1,x2) = parse 'T':st
    
--branjeOmejitve ('K':st:'x':kot) = 
--branjeOmejitve ('P':st:'t') = 
--branjeOmejitve ('P':st:'x':kot) = 

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
split c = wordsWhen (==c)
test = split ',' "break,this,string,at,commas"

branjePodatka "" = ""
branjePodatka ('[':ostanek) = branjePodatka2 ostanek
branjePodatka (x:ostanek) = branjePodatka ostanek

branjePodatka2 "" = ""
branjePodatka2 (']':ostanek) ='$':(branjePodatka ostanek)
branjePodatka2 (x:ostanek) = x:(branjePodatka2 ostanek)

--razbijPodatekNaKose niz =print $ split ' ' niz

branjePodatkov besedilo = split '$' (branjePodatka besedilo)

fff x=split ' ' x

razdelitevPodatkov besedilo = map fff (branjePodatkov besedilo)

obdrziZanimive a b c (glava:podatkiEnega)
    |a==glava =True
    |b==glava =True
    |c==glava =True
    |otherwise =False

predelajZanimive [a,a1] [b,b1] [c,c1] (glava:x:podatkiEnega)
    |a==glava =[a,a1]++podatkiEnega
    |b==glava =[b,b1]++podatkiEnega
    |c==glava =[c,c1]++podatkiEnega
    |otherwise =[]
    
vrniPomembnePodatke a b c besedilo = [x |x<-(razdelitevPodatkov besedilo), obdrziZanimive a b c x]


razbijOmejitve niz=map (split ';') (split '|' niz)

seznamVfunkcijo [[a,a1],[b,b1],[c,c1]] besedilo = vrniPomembnePodatke a b c besedilo

-- ce gre skozi tocko
napisiOmejitev [_,"s",niz] = passing_through x y
    where
        (x,y) = read niz
        
napisiOmejitev ['K':ost,"t",niz1,niz2] = tangent_to (clockwise_circle x y r)
    where
        (x,y) = read niz1
        r = read niz2

napisiOmejitev ['P':ost,"t",niz1,niz2] = tangent_to (line x0 y0 x1 y1) --naredi premico
    where
        (x0,y0) = read niz1
        (x1,y1) = read niz2      
        

sklop [a,a1] [b,b1] [c,c1] podatkiEnega= napisiOmejitev (predelajZanimive [a,a1] [b,b1] [c,c1] podatkiEnega)
mapVecSklop :: [[[Char]]] -> [Char] -> [Circle Unknown -> Results]
mapVecSklop [[a,a1],[b,b1],[c,c1]] besedilo = map (sklop [a,a1] [b,b1] [c,c1])  (vrniPomembnePodatke a b c besedilo)
        
poisciKrogKiZadoscaOmejitvamInBesedilu :: [Char] -> [Char] -> Circle Known -> Circle Known
poisciKrogKiZadoscaOmejitvamInBesedilu nizOmejitev besedilo aktualniPodatek = find aktualniPodatek $ satisfying sezOmejitev
    where
        sezOmejitev=mapVecSklop (razbijOmejitve nizOmejitev) besedilo



        
-- oblika K1;t|K2;s|K3;t


--vrniPodatke besedilo =map (print $ split ']') (print $ split '[' niz)

osnovniPretvornik1 :: Distance -> Float
osnovniPretvornik1 a = read(show(a))

osnovniPretvornik2 ::  Float -> Distance
osnovniPretvornik2 a = read(show(a))

osnovniPretvornik3 :: Distance -> Double
osnovniPretvornik3 a = read(show(a))

osnovniPretvornik4 :: Double -> Distance
osnovniPretvornik4 a = read(show(a))

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
        
spremembaObjekta :: (CanvasItem b) =>[Char] -> Distance -> Distance -> Distance -> Distance -> Config b
spremembaObjekta "Kroznica" x0 y0 x1 y1 = coord [(xk1,yk1),(xk2,yk2)]
    where
        (xk1,xk2,yk1,yk2) = pretvorbaOmejenaRadialna x0 y0 x1 y1

spremembaObjekta "Daljica" x0 y0 x1 y1 = coord [(x0,y0),(x1,y1)]
spremembaObjekta "Premica" x0 y0 x1 y1 = coord [(xk1,yk1),(xk2,yk2)]
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
        

pretvoriVracunskoObliko2:: Distance -> Distance -> Distance -> Distance -> [Char]-> Circle Known
pretvoriVracunskoObliko2 x0 y0 x1 y1 "Kroznica"= anticlockwise_circle xs ys r
    where
        x0fl=(osnovniPretvornik3 x0) ::Double
        y0fl=(osnovniPretvornik3 y0) ::Double
        x1fl=(osnovniPretvornik3 x1) ::Double
        y1fl=(osnovniPretvornik3 y1) ::Double
        r=sqrt((x1fl-x0fl)^2+(y1fl-y0fl)^2)
        (xs,ys)=(x0fl,y0fl)
        
pretvoriVracunskoObliko :: Distance -> Distance -> Distance -> Distance -> [Char]-> Circle Known
pretvoriVracunskoObliko x0 y0 x1 y1 "Kroznica"= clockwise_circle xs ys r
    where
        x0fl=(osnovniPretvornik3 x0) ::Double
        y0fl=(osnovniPretvornik3 y0) ::Double
        x1fl=(osnovniPretvornik3 x1) ::Double
        y1fl=(osnovniPretvornik3 y1) ::Double
        r=sqrt((x1fl-x0fl)^2+(y1fl-y0fl)^2)
        (xs,ys)=(x0fl,y0fl)
        
pretvoriIzRacunskeOblike :: Circle Known -> (Distance,Distance,Distance,Distance)
pretvoriIzRacunskeOblike kRacunska =pretvoriIzStandardneOblike (tr_racunska_standrdna kRacunska)

pretvoriIzStandardneOblike :: OsnovniObjekt Double -> (Distance,Distance,Distance,Distance)
pretvoriIzStandardneOblike (Kroznica (x,y) r) =(x0a,y0a,x1a,y1a)  --(x0,y0,x1,y1)
    where
        x0fl=x-r
        y0fl=y-r
        x1fl=x+r
        y1fl=y+r
        x0a=(osnovniPretvornik4 x0fl) ::Distance
        y0a=(osnovniPretvornik4 y0fl) ::Distance
        x1a=(osnovniPretvornik4 x1fl) ::Distance
        y1a=(osnovniPretvornik4 y1fl) ::Distance
        --[x0,y0,x1,y1]=map osnovniPretvornik4 [x0fl,y0fl,x1fl,y1fl]
        
        
-- sestaviBesediloElementa
sestaviBesediloElementa :: [Char] -> Distance -> Distance -> Distance -> Distance -> Distance -> [Char] -> [Char]
sestaviBesediloElementa "Kroznica" x0 y0 x1 y1 zaporednaSt dolocenost= razsirjeniNizObKor
    where
        x0fl=(osnovniPretvornik1 x0) ::Float
        y0fl=(osnovniPretvornik1 y0) ::Float
        x1fl=(osnovniPretvornik1 x1) ::Float
        y1fl=(osnovniPretvornik1 y1) ::Float
        r=sqrt((x1fl-x0fl)^2+(y1fl-y0fl)^2)
        (xs,ys)=(x0fl,y0fl)
        razsirjeniNizObKor = ""++show(dolocenost)++";[K"++show(zaporednaSt)++" Kroznica "++show((xs,ys))++" "++show(r)++"];//;"
        
      

-- sestaviBesediloElementa "Premica" x0 y0 x1 y1 zaporednaSt dolocenost= razsirjeniNizObKor
    -- where
        -- x0fl=osnovniPretvornik1 x0 ::Float
        -- y0fl=osnovniPretvornik1 y0 ::Float
        -- x1fl=osnovniPretvornik1 x1 ::Float
        -- y1fl=osnovniPretvornik1 y1 ::Float
        -- sprX=x0fl-x1fl
        -- sprY=y0fl-y1fl
        -- k=sprY/sprX
        -- n=y0fl-k*x0fl
        -- razsirjeniNizObKor = ""++show(dolocenost)++";[P"++show(zaporednaSt)++" Premica y=k*x+n k="++show(k)++" n="++show(n)++"];//;"

sestaviBesediloElementa "Premica" x0 y0 x1 y1 zaporednaSt dolocenost= razsirjeniNizObKor
    where
        x0fl=osnovniPretvornik1 x0 ::Float
        y0fl=osnovniPretvornik1 y0 ::Float
        x1fl=osnovniPretvornik1 x1 ::Float
        y1fl=osnovniPretvornik1 y1 ::Float
        razsirjeniNizObKor = ""++show(dolocenost)++";[P"++show(zaporednaSt)++" Premica "++show((x0fl,y0fl))++" "++show((x1fl,y1fl))++"];//;"
        
sestaviBesediloElementa "Daljica" x0 y0 x1 y1 zaporednaSt dolocenost= razsirjeniNizObKor
    where
        x0fl=osnovniPretvornik1 x0 ::Float
        y0fl=osnovniPretvornik1 y0 ::Float
        x1fl=osnovniPretvornik1 x1 ::Float
        y1fl=osnovniPretvornik1 y1 ::Float
        razsirjeniNizObKor = ""++show(dolocenost)++";[D"++show(zaporednaSt)++" Daljica "++show((x0fl,y0fl))++" "++show((x1fl,y1fl))++"];//;"

sestaviBesediloElementa "Tocka" x0 y0 _ _ zaporednaSt dolocenost= razsirjeniNizObKor
    where
        x0fl=osnovniPretvornik1 x0 ::Float
        y0fl=osnovniPretvornik1 y0 ::Float
        razsirjeniNizObKor = ""++show(dolocenost)++";[T"++show(zaporednaSt)++" Tocka "++show((x0fl,y0fl))++"];//;"

-- dodatekStevilaObjektov "Daljica" (a,b,c,d) = (a,b,c,d+1)        
-- dodatekStevilaObjektov "Kroznica" (a,b,c,d) = (a,b,c+1,d)
-- dodatekStevilaObjektov "Premica" (a,b,c,d) = (a,b+1,c,d)
-- dodatekStevilaObjektov "Tocka" (a,b,c,d) = (a+1,b,c,d)       
        
-- funkcijaIzbire "Premica" = createLine
-- funkcijaIzbire "Kroznica" = createOval
-- funkcijaIzbire "Daljica" = funkcijaIzbire "Premica"

-- ustvariObjekt platno "Premica" x0 y0 x1 y1 = rrr
    -- where
        -- (
        -- do
        -- rrr <- createLine platno [coord [(x0,y0),(x1,y1)], filling "green"]
        -- done
        -- )
        
-- ustvariObjekt platno "Kroznica" x0 y0 x1 y1 = rrr
    -- where
        -- (
        -- do
        -- (xk1,xk2,yk1,yk2) = pretvorbaOmejenaRadialna x0 y0 x1 y1
        -- rrr <- createOval platno [coord [(xk1,yk1),(xk2,yk2)], filling "green"]
        -- done
        -- )

ustvariOdGumba :: Canvas -> [Char] -> IO() --([Char], Line)--IO()
ustvariOdGumba platno "Daljica" =(
    do
    podatki <- createLine platno [coord [(0,0),(0,0)], filling "red"]
    done
    --let spreminjajociSeObjekt = ("Premica",podatki)
    --spreminjajociSeObjekt
    )

novMain=main
	
main::IO()
main =
    do
    main <- initHTk [text "Dinamicna geometrija"]

    oo00 <- newFrame main []
    grid oo00 [GridPos (1,1),GridPadX 1,GridPadY 1]	
	
    oo02 <- newFrame oo00 []
    grid oo02 [GridPos (2,1),GridPadX 1,GridPadY 1]
	
    urejevalnikBesedila <- newEditor oo02 [size (80,40)] --, state Enabled]--Disabled]
    grid urejevalnikBesedila [GridPos (1,1),GridPadX 1,GridPadY 1]
    
    zaklenjenIzhod <- (newEntry oo02 [value "0", width 20])::IO (Entry String) --, state Disabled])::IO (Entry String)
    --grid zaklenjenIzhod [GridPos (1,2),GridPadX 1,GridPadY 1]  ----To izgleda smesno vendar je se najcenejsa globalna spremenljivka

    --zaklenjenIzhodTKPD <- (newEntry oo02 [value "(0,0,0,0)", width 20])::IO (Entry String) --, state Disabled])::IO (Entry String)
    --grid zaklenjenIzhodTKPD [GridPos (1,2),GridPadX 1,GridPadY 1]  ----To izgleda smesno vendar je se najcenejsa globalna spremenljivka
    
    zaklenjenIzhodT <- (newEntry oo02 [value "1", width 20])::IO (Entry String) --, state Disabled])::IO (Entry String)
    --grid zaklenjenIzhodT [GridPos (1,2),GridPadX 1,GridPadY 1]  ----To izgleda smesno vendar je se najcenejsa globalna spremenljivka
    
    zaklenjenIzhodK <- (newEntry oo02 [value "1", width 20])::IO (Entry String) --, state Disabled])::IO (Entry String)
    --grid zaklenjenIzhod [GridPos (1,2),GridPadX 1,GridPadY 1]  ----To izgleda smesno vendar je se najcenejsa globalna spremenljivka
    
    zaklenjenIzhodP <- (newEntry oo02 [value "1", width 20])::IO (Entry String) --, state Disabled])::IO (Entry String)
    --grid zaklenjenIzhod [GridPos (1,2),GridPadX 1,GridPadY 1]  ----To izgleda smesno vendar je se najcenejsa globalna spremenljivka
    
    zaklenjenIzhodD <- (newEntry oo02 [value "1", width 20])::IO (Entry String) --, state Disabled])::IO (Entry String)
    --grid zaklenjenIzhod [GridPos (1,2),GridPadX 1,GridPadY 1]  ----To izgleda smesno vendar je se najcenejsa globalna spremenljivka
    
    vhodTeksta <- (newEntry oo02 [value "0", width 20])::IO (Entry String) --, state Disabled])::IO (Entry String)
    --grid zaklenjenIzhod [GridPos (1,2),GridPadX 1,GridPadY 1]  ----To izgleda smesno vendar je se najcenejsa globalna spremenljivka
   
    vhodZaRelacije <- (newEntry oo02 [value "", width 80])::IO (Entry String) --, state Disabled])::IO (Entry String)
    grid vhodZaRelacije [GridPos (1,3),GridPadX 1,GridPadY 1]  ----To izgleda smesno vendar je se najcenejsa globalna spremenljivka

   
    let dopolniFunkcijo "Daljica" = zaklenjenIzhodD
        dopolniFunkcijo "Kroznica" = zaklenjenIzhodK
        dopolniFunkcijo "Premica" = zaklenjenIzhodP
        dopolniFunkcijo "Tocka" = zaklenjenIzhodT
    
    
    oo01 <- newFrame oo00 []
    grid oo01 [GridPos (1,1),GridPadX 1,GridPadY 1]	
	
    oo2 <- newFrame oo01 []
    oo3 <- newFrame oo01 []
	
    grid oo2 [GridPos (1,1),GridPadX 1,GridPadY 1]
    grid oo3 [GridPos (1,2),GridPadX 1,GridPadY 1]

    let dimenzijePlatna=(800,600)
    platno <- newCanvas oo2 [size dimenzijePlatna, background "white"]
    grid platno [GridPos (1,1),GridPadX 1,GridPadY 1]
	
    -- createLine platno [coord [(1,1),(100,100)]]
    -- createLine platno [coord [(200,1),(300,100)], filling "red"]
    -- createLine platno [coord [(200,400),(300,400)]]
    
    -- createArc platno [position (500, 500), size (100,200), filling "green", outlinewidth 3, outline "black"]	
    -- createArc platno [position (200, 200), size (400,200), filling "yellow", outlinewidth 1, outline "black"]
    
    --xxx <- createOval platno [coord [(77,77),(222,222)], filling "blue"]
    -- createOval platno [coord [(400,100),(222,222)], filling "blue"]
    
    -- podatki <- createOval platno [coord [(0,0),(0,0)],filling "white"]
    
    -- podatki
    
    -- mojaGlobalna :: IORef
    -- mojaGlobalna = unsafePerformIO (newIORef 17)
    
    -- let spreminjajociSeObjekt = globalizirajSpremenljivko ("Kroznica", podatki)-- ::CanvasItem)
    -- podatki <- createLine platno [coord [(0,0),(0,0)], filling "red"]
    -- withTag podatki
    -- let spreminjajociSeObjekt = globalizirajSpremenljivko ("Daljica", podatki)-- ::CanvasItemKind)--)-- ::(HasCoords a))
    
    -- let testBesedilo="zacetno"
    -- always (createOval platno [coord [(400,100),(222,222)], filling "green"])
    
    let
      link :: String -> MarkupText
      link str = flipcolour "#4756ff" "#4c90ff"
                   [rangeaction (Just (urejevalnikBesedila # cursor arrow >> done))
                                (Just (urejevalnikBesedila # cursor xterm >> done))
                   [flipunderline [prose str]]]
    
    let besediloVmestnika::[MarkupText]
        besediloVmestnika =
            [prose "Tukaj se izpisejo podatki iz platna:",
            newline
            ]

    urejevalnikBesedila # new besediloVmestnika
    
    gumb1 <- newButton oo3 [text "Pocisti platno"]
    gumb2 <- newButton oo3 [text "Zapri"]
    gumb3 <- newButton oo3 [text "Tocka"]
    gumb4 <- newButton oo3 [text "Premica"]
    gumb5 <- newButton oo3 [text "Kroznica"]
    gumb6 <- newButton oo3 [text "Daljica"]
    gumb7 <- newButton oo3 [text "Preberi vse"]
    gumb8 <- newButton oo3 [text "Nedolocen objekt"]
    gumb9 <- newButton oo3 [text "Razresi relacije"]
    
    klikg1 <- clicked gumb1
    klikg2 <- clicked gumb2
    klikg3 <- clicked gumb3
    klikg4 <- clicked gumb4
    klikg5 <- clicked gumb5
    klikg6 <- clicked gumb6
    klikg7 <- clicked gumb7
    klikg8 <- clicked gumb8
    klikg9 <- clicked gumb9

    (klikMiskaM1, _) <- bind platno [WishEvent [] (ButtonPress (Just 1))]
    (premikPoKlikM1, _) <- bind platno [WishEvent [Button1] Motion]
    (navadniPremik, _) <- bind platno [WishEvent [] Motion]
    (odKlikMiskaM1, _) <- bindSimple platno (ButtonRelease (Just 1))
    
    let sezG = [gumb1,gumb8,gumb3,gumb4,gumb5,gumb6,gumb7,gumb2,gumb9]
    delayWish $ mapM_ (\(gumb,(x,y)) -> grid gumb [GridPos (x,y),GridPadX 1,GridPadY 1]) (zip sezG [(x,1)|x <-[1..100]])
    
    let premikanjeMiskePoKlik :: Distance -> Distance -> Distance -> Distance -> [Char] -> CanvasTag -> [Char] -> Event ()
        premikanjeMiskePoKlik x y _ _ "Tocka" oznakaP dolocenost=(odKlikMiskaM1 >> odKlikMiske x y x y "Tocka" oznakaP dolocenost)-- !!!tu bu narejeno da se bo klicala nasa funkcija ---odKlikMiske x0 y0 x1 y1 nizObjekta oznakaP
        premikanjeMiskePoKlik x0 y0 x1 y1 nizObjekta oznakaP dolocenost =(
            (
            do
            (x, y) <- premikPoKlikM1 >>>= \i-> return (x i, y i)
            always (gumb1 # text ("(x,y): " ++ show (x,y) ++ "  (x0,y0): " ++ show (x0,y0)))
            let (xk1,xk2,yk1,yk2) = pretvorbaOmejenaRadialna x0 y0 x y
            always(oznakaP # (spremembaObjekta nizObjekta x0 y0 x1 y1))
            premikanjeMiskePoKlik x0 y0 x y nizObjekta oznakaP dolocenost
            )
            +>
            (
            odKlikMiskaM1 >> odKlikMiske x0 y0 x1 y1 nizObjekta oznakaP dolocenost--done-- !!!tu bu narejeno da se bo klicala nasa funkcija ---odKlikMiske x0 y0 x1 y1 nizObjekta oznakaP
            )
            )
            
        odKlikMiske :: Distance -> Distance -> Distance -> Distance -> [Char] -> CanvasTag -> [Char] -> Event ()
        -- odKlikMiske x0 y0 x1 y1 "TockaPredklic" oznakaP = (
            -- do
            -- odKlikMiske x0 y0 x1 y1 "Tocka" oznakaP
            -- always(
                    -- do
                    -- nizStevilaObjektov <- (getValue zaklenjenIzhodT) :: IO String
                    -- let steviloObjektov = (read nizStevilaObjektov)::Distance
                    -- zaklenjenIzhodT # value (show steviloObjektov)
                    -- done
                    -- )
                    
            -- done
            -- )
        odKlikMiske x0 y0 x1 y1 nizObjekta oznakaP dolocenost= (
            do
            steviloObjektov <- always(
                    do
                    nizStevilaObjektov <- (getValue zaklenjenIzhod) :: IO String
                    let steviloObjektov = (read nizStevilaObjektov)::Distance
                    zaklenjenIzhod # value (show (steviloObjektov+1))
                    return steviloObjektov
                    )
            always(putStrLn nizObjekta)
            zaporednaSt <- odKlikMiskePomozna nizObjekta
            
            always(urejevalnikBesedila # insertAt (
                [action (
                do
                nizOmejitev <- (getValue vhodZaRelacije) :: IO String
                besedilo <- (getValue urejevalnikBesedila) :: IO String
                done
                putStrLn nizOmejitev
                putStrLn besedilo
                --putStrLn (show (poisciKrogKiZadoscaOmejitvamInBesedilu nizOmejitev besedilo (clockwise_circle 209 351 42.5)))
                let xxx=poisciKrogKiZadoscaOmejitvamInBesedilu nizOmejitev besedilo (pretvoriVracunskoObliko x0 y0 x1 y1 nizObjekta)
                putStrLn (show (xxx))
                let (x0,y0,x1,y1)=pretvoriIzRacunskeOblike xxx
                oznakaP # coord[(x0,y0),(x1,y1)]
                putStrLn (show (x0,y0,x1,y1))
                putStrLn "fffggg"

                --poisciKrogKiZadoscaOmejitvamInBesedilu nizOmejitev besedilo (clockwise_circle 209 351 42.5)
                -- primer T1;s|T2;s|T3;s
                -- primer K1;t|K2;t|K3;t
                -- primer K1;t|K2;t|T1;s
                
                -- P1;t|P2;t|P3;t
                )
                --putStrLn (show (x0,y0))) 
                [link (sestaviBesediloElementa nizObjekta x0 y0 x1 y1 zaporednaSt dolocenost)]
                
                ,
                action (
                do
                nizOmejitev <- (getValue vhodZaRelacije) :: IO String
                besedilo <- (getValue urejevalnikBesedila) :: IO String
                done
                putStrLn nizOmejitev
                putStrLn besedilo
                --putStrLn (show (poisciKrogKiZadoscaOmejitvamInBesedilu nizOmejitev besedilo (clockwise_circle 209 351 42.5)))
                let xxx=poisciKrogKiZadoscaOmejitvamInBesedilu nizOmejitev besedilo (pretvoriVracunskoObliko2 x0 y0 x1 y1 nizObjekta)
                
                let (x0,y0,x1,y1)=pretvoriIzRacunskeOblike xxx
                oznakaP # coord[(x0,y0),(x1,y1)]
                putStrLn (show (xxx))
                --putStrLn (show (x0,y0,x1,y1))
                --putStrLn "fffggg"

                --poisciKrogKiZadoscaOmejitvamInBesedilu nizOmejitev besedilo (clockwise_circle 209 351 42.5)
                -- primer T1;s|T2;s|T3;s
                -- primer K1;t|K2;t|K3;t
                -- primer K1;t|K2;t|K3;t
                -- primer T1;s|T2;s|P1;t
                )
                [link "ANTI"]
                ,newline
                
                ]) 
                (2+steviloObjektov,0)
                )
            done
            )
        odKlikMiskePomozna nizObjekta = (
            do
            steviloObjektov <- always(
                    do
                    nizStevilaObjektov <- (getValue (dopolniFunkcijo nizObjekta)) :: IO String
                    let steviloObjektov = (read nizStevilaObjektov)::Distance
                    (dopolniFunkcijo nizObjekta) # value (show (steviloObjektov+1))
                    return steviloObjektov
                    )
            return steviloObjektov
            )
        
            -- do
            --always(xxx # coord [(77,77),(222,555)])
            --let (xk1,xk2,yk1,yk2) = pretvorbaOmejenaRadialna x0 y0 x1 y1
            --always (createOval platno [coord [(xk1,yk1),(xk2,yk2)], outlinewidth 3, outline "black"])
            --always (createOval platno [coord [(x0,y0),(x1,y1)], filling "red"])
            --klikMiske nizObjekta
            -- )
        
        klikMiske :: [Char] -> [Char] -> Event ()
        klikMiske "Kroznica" dolocenost= (
            do
            (x, y) <- klikMiskaM1 >>>= \i-> return (x i, y i)
            objektNOV <- always(
                            do
                            objektNOV <- createOval platno [coord[(0,0),(0,0)]]
                            return objektNOV
                            )
            oznakaP <- always (
                            do
                            oznakaP<- createCanvasTag platno []
                            addCanvasTag (withTag objektNOV) oznakaP
                            return oznakaP
                            )
            premikanjeMiskePoKlik x y x y "Kroznica" oznakaP dolocenost
            )
    
        klikMiske "Daljica" dolocenost= (
            do
            (x, y) <- klikMiskaM1 >>>= \i-> return (x i, y i)
            objektNOV <- always(
                            do
                            objektNOV <- createLine platno [coord[(0,0),(0,0)]]
                            return objektNOV
                            )
            oznakaP <- always (
                            do
                            oznakaP<- createCanvasTag platno []
                            addCanvasTag (withTag objektNOV) oznakaP
                            return oznakaP
                            )
            premikanjeMiskePoKlik x y x y "Daljica" oznakaP dolocenost
            )

        klikMiske "Premica" dolocenost= (
            do
            (x, y) <- klikMiskaM1 >>>= \i-> return (x i, y i)
            objektNOV <- always(
                            do
                            objektNOV <- createLine platno [coord[(0,0),(0,0)]]
                            return objektNOV
                            )
            oznakaP <- always (
                            do
                            oznakaP<- createCanvasTag platno []
                            addCanvasTag (withTag objektNOV) oznakaP
                            return oznakaP
                            )
            premikanjeMiskePoKlik x y x y "Premica" oznakaP dolocenost
            )
            
        klikMiske "Tocka" dolocenost= (
            do
            (x, y) <- klikMiskaM1 >>>= \i-> return (x i, y i)
            --do
            objektNOV <- always(
                    do
                    let r=2--originalno 1
                    tocka <- createOval platno [coord[(x-r,y-r),(x+r,y+r)], filling "red"]
                    objektNOV <- createTextItem platno [position (x+20, y), text "TOCKA"]
                    nizStevilaObjektov <- (getValue zaklenjenIzhodT) :: IO String
                    let steviloObjektov = (read nizStevilaObjektov)::Distance
                    objektNOV # text ("T" ++ show steviloObjektov)
                    return objektNOV
                    )
            oznakaP <- always (
                            do
                            oznakaP <- createCanvasTag platno []
                            addCanvasTag (withTag objektNOV) oznakaP
                            return oznakaP
                            )
            premikanjeMiskePoKlik x y x y "Tocka" oznakaP dolocenost
            )

            
    _ <- spawnEvent(forever (
                        (klikg1 >> 
						do
						always(novMain)
						always(destroy main)
                        -- napisg1 <- mapM randomRIO (replicate 5 ('a','z'))
                        -- gumb1 # text napisg1
                        )
                        +>
                        (klikg2 >>> destroy main)
                        +>
                        (klikg3 >> klikMiske "Tocka" "Dolocen"
                        )
                        +>
                        (klikg4 >> klikMiske "Premica" "Dolocen"
                        )
                        +>
                        (klikg5 >> klikMiske "Kroznica" "Dolocen"
                        )
                        +>
                        (klikg6 >> klikMiske "Daljica" "Dolocen"
                        )
                        +>
                        (klikMiskaM1 >> done
                        )
                        +>
                        (odKlikMiskaM1 >> done
                        )
                        +>
                        (
                        do
                        (x, y) <- navadniPremik >>>= \i-> return (x i, y i)
                        always (gumb1 # text ("(x,y): " ++ show (x,y)))
                        done
                        )
                        +>
                        (
                        klikg7>> (
                            do
                            celotnoBesedilo000 <-always(do 
                                    celotnoBesedilo000 <- (getValue urejevalnikBesedila) :: IO String
                                    return celotnoBesedilo000
                                    )
                            always(putStrLn celotnoBesedilo000)
                            done
                            )
                        )
                        +>
                        (
                        klikg8>> (
                                (
                                klikg3 >> klikMiske "Tocka" "Nedolocen"
                                )
                                +>
                                (
                                klikg4 >> klikMiske "Premica" "Nedolocen"
                                )
                                +>
                                (
                                klikg5 >> klikMiske "Kroznica" "Nedolocen"
                                )
                                +>
                                (
                                klikg6 >> klikMiske "Daljica" "Nedolocen"
                                )
                                    )
                        )
                        +>
                        (
                        klikg9>> (
                            do
                            nizRelacij <-always(do 
                                    nizRelacij <- (getValue vhodZaRelacije) :: IO String
                                    return nizRelacij
                                    )
                            always(putStrLn nizRelacij)--(preberiPodatke nizRelacij))
                            done
                                    )
                        )
                        )
					)
                                        
    
    -- vrsta vnosa K1 [(K2,t), (T3,s), (K4,t)]
    -- spawnEvent (forever (
                        -- klikMiske
                        -- )
                -- )
    finishHTk
