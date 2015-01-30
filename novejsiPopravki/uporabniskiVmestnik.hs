import HTk.Toplevel.HTk
import HTk.Toolkit.MarkupText as MarkupText
import Circle
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



novMain=main

main::IO()
main =
    do
    main <- initHTk [text "Dinamicna geometrija"]

    oo00 <- newFrame main []
    grid oo00 [GridPos (1,1),GridPadX 1,GridPadY 1]	
	
    oo02 <- newFrame oo00 []
    grid oo02 [GridPos (2,1),GridPadX 1,GridPadY 1]
	
    -- ni ista enota pri size
    urejevalnikBesedila <- newEditor oo02 [size (80,25)]
    grid urejevalnikBesedila [GridPos (1,1),GridPadX 1,GridPadY 1]
    
    urejevalnikBesedilaRezultat <- newEditor oo02 [size (80,15)]
    grid urejevalnikBesedilaRezultat [GridPos (1,2),GridPadX 1,GridPadY 1]
    
    vhodZaRelacije <- (newEntry oo02 [value "", width 80])::IO (Entry String) 
    grid vhodZaRelacije [GridPos (1,3),GridPadX 1,GridPadY 1]
    
    oo01 <- newFrame oo00 []
    grid oo01 [GridPos (1,1),GridPadX 1,GridPadY 1]	
	
    oo2 <- newFrame oo01 []
    oo3 <- newFrame oo01 []
	
    grid oo2 [GridPos (1,1),GridPadX 1,GridPadY 1]
    grid oo3 [GridPos (1,2),GridPadX 1,GridPadY 1]

    -- trenutno je celoten program narejen na dimenzijo mojega zaslona (ni tezko spremeniti)
    let dimenzijePlatna=(800,600)
    platno <- newCanvas oo2 [size dimenzijePlatna, background "white"]
    grid platno [GridPos (1,1),GridPadX 1,GridPadY 1]
    
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

    let sezG = [gumb1,gumb8,gumb3,gumb4,gumb5,gumb6,gumb7,gumb2,gumb9]
    -- Naslednja funkcija delayWish je pri hitrosti danasnjih racunalnikov mogoce odvec (vse gumbe da gor na enkrat)
    delayWish $ mapM_ (\(gumb,(x,y)) -> grid gumb [GridPos (x,y),GridPadX 1,GridPadY 1]) (zip sezG [(x,1)|x <-[1..100]])
    
    zaklenjenIzhod <- (newEntry oo02 [value "0", width 20])::IO (Entry String)
    zaklenjenIzhodT <- (newEntry oo02 [value "1", width 20])::IO (Entry String)
    zaklenjenIzhodK <- (newEntry oo02 [value "1", width 20])::IO (Entry String)
    zaklenjenIzhodP <- (newEntry oo02 [value "1", width 20])::IO (Entry String)
    zaklenjenIzhodD <- (newEntry oo02 [value "1", width 20])::IO (Entry String)
    --grid zaklenjenIzhod [GridPos (1,2),GridPadX 1,GridPadY 1]
    --To izgleda smesno vendar je se najcenejsa globalna spremenljivka
    
    
    let dopolniFunkcijo "Daljica" = zaklenjenIzhodD
        dopolniFunkcijo "Kroznica" = zaklenjenIzhodK
        dopolniFunkcijo "Premica" = zaklenjenIzhodP
        dopolniFunkcijo "Tocka" = zaklenjenIzhodT
  
    let link :: String -> MarkupText
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
    
    
    let besediloVmestnikaRezultat::[MarkupText]
        besediloVmestnikaRezultat =
            [prose "Tukaj se izpisejo rezultati izracunov:",
            newline
            ]
            
    urejevalnikBesedilaRezultat # new besediloVmestnikaRezultat
    
    let narisiObjekt (Kroznica (x,y) r) = createOval platno [coord[(x1,y1),(x2,y2)], outline "grey"]
            where 
                (x1,y1,x2,y2) = pretvoriIzStandardneOblike (Kroznica (x,y) r)

        narisiObjekt (Tocka (x1,y1)) = createOval platno [coord[(x1a-1,y1a-1),(x1a+1,y1a+1)], outline "grey"]
            where
                x1a=(osnovniPretvornik4 x1) ::Distance
                y1a=(osnovniPretvornik4 y1) ::Distance
                
        -- narisiObjekt (Premica (x1,y1) (x2,y2)) = createLine platno [coord[(x1a,y1a),(x2a,y2a)]]
            -- where
                -- x1a=(osnovniPretvornik4 x1) ::Distance
                -- y1a=(osnovniPretvornik4 y1) ::Distance
                -- x2a=(osnovniPretvornik4 x2) ::Distance
                -- y2a=(osnovniPretvornik4 y2) ::Distance
                
    (klikMiskaM1, _) <- bind platno [WishEvent [] (ButtonPress (Just 1))]
    (premikPoKlikM1, _) <- bind platno [WishEvent [Button1] Motion]
    (odKlikMiskaM1, _) <- bindSimple platno (ButtonRelease (Just 1))
    (navadniPremik, _) <- bind platno [WishEvent [] Motion]
    

    
    let klikMiske :: [Char] -> Event () -- to se mi zdi zamudno(dolgovezno) delati z vzorci (patern match) se da hitreje?
        klikMiske "Kroznica" = (
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
            premikanjeMiskePoKlik x y x y "Kroznica" oznakaP
            )
    
        klikMiske "Daljica" = (
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
            premikanjeMiskePoKlik x y x y "Daljica" oznakaP
            )

        klikMiske "Premica" = (
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
            premikanjeMiskePoKlik x y x y "Premica" oznakaP
            )
            
        klikMiske "Tocka" = (
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
            premikanjeMiskePoKlik x y x y "Tocka" oznakaP
            )
    
        premikanjeMiskePoKlik :: Distance -> Distance -> Distance -> Distance -> [Char] -> CanvasTag -> Event ()
        premikanjeMiskePoKlik x y _ _ "Tocka" oznakaP =(odKlikMiskaM1 >> odKlikMiske x y x y "Tocka" oznakaP)
        premikanjeMiskePoKlik x0 y0 x1 y1 nizObjekta oznakaP =(
            (
            do
            (x, y) <- premikPoKlikM1 >>>= \i-> return (x i, y i)
            always (gumb1 # text ("(x,y): " ++ show (x,y) ++ "  (x0,y0): " ++ show (x0,y0)))
            --let (xk1,xk2,yk1,yk2) = pretvorbaOmejenaRadialna x0 y0 x y
            --let [(xk1,yk1),(xk2,yk2)] = spremembaObjektaP "Kroznica" x0 y0 x y
            always(oznakaP # (spremembaObjekta nizObjekta x0 y0 x1 y1))
            premikanjeMiskePoKlik x0 y0 x y nizObjekta oznakaP
            )
            +>
            (
            odKlikMiskaM1 >> odKlikMiske x0 y0 x1 y1 nizObjekta oznakaP
            )
            )
        
        odKlikMiske :: Distance -> Distance -> Distance -> Distance -> [Char] -> CanvasTag -> Event ()
        odKlikMiske x0 y0 x1 y1 nizObjekta oznakaP = (
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
                
                let sezObjektov=poisciUstrezneKroge nizOmejitev besedilo (standardizirajObliko nizObjekta x0 y0 x1 y1)
                putStrLn (show (sezObjektov))
                
                mapM_ narisiObjekt sezObjektov

                let sez =[prose "Tukaj se izpisejo rezultati izracunov:", newline] ++ [prose (unlines (map show (sezObjektov)))]
                let besediloVmestnikaRezultat = sez

                do
                urejevalnikBesedilaRezultat # new besediloVmestnikaRezultat
                done

                
                )
                --putStrLn (show (x0,y0))) 
                [link (sestaviBesediloElementa nizObjekta x0 y0 x1 y1 zaporednaSt)]
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
        
    let popravljajKoordinate = (
            do
            (x, y) <- navadniPremik >>>= \i-> return (x i, y i)
            always (gumb1 # text ("(x,y): " ++ show (x,y)))
            done
            )    
    
    _ <- spawnEvent(forever (
                        (klikg1 >> 
						do
						always(novMain)
						always(destroy main)
                        )
                        +>
                        (klikg2 >>> destroy main)
                        +>
                        (klikg3 >> (klikMiske "Tocka")--((klikMiske "Tocka") +> popravljajKoordinate)
                        )
                        +>
                        (klikg4 >> (klikMiske "Premica")--((klikMiske "Premica") +> popravljajKoordinate)
                        )
                        +>
                        (klikg5 >> (klikMiske "Kroznica")--((klikMiske "Kroznica") +> popravljajKoordinate)
                        )
                        +>
                        (klikg6 >> (klikMiske "Daljica")--((klikMiske "Daljica") +> popravljajKoordinate)
                        )
                        +>
                        (klikMiskaM1 >> done
                        )
                        +>
                        (odKlikMiskaM1 >> done
                        )
                        +>
                        popravljajKoordinate
                        -- (
                        -- do
                        -- (x, y) <- navadniPremik >>>= \i-> return (x i, y i)
                        -- always (gumb1 # text ("(x,y): " ++ show (x,y)))
                        -- done
                        -- )
                            )
                    )

    
    finishHTk
