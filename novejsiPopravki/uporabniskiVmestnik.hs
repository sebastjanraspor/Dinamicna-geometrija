import HTk.Toplevel.HTk
import HTk.Toolkit.MarkupText as MarkupText
import Circle
import StrukturniObjekti
import PretvorbePodatkov
import NarediPDF
-- import HTk.Kernel.GridPackOptions

novMain=main

--oknoLat::IO()

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
    
    
    let oknoLat =
            do
            oknoLat <- newFrame oo00 []
            grid oknoLat [GridPos (2,1),GridPadX 1,GridPadY 1, Sticky NSEW]
        
            besedilo <- (getValue urejevalnikBesedila) :: IO String
            
            besediloRezultat <- (getValue urejevalnikBesedilaRezultat) :: IO String
        
            urejevalnikLat <- newEditor oknoLat [size (80, 40)]
            grid urejevalnikLat [GridPos (1,1),GridPadX 1,GridPadY 1]
        
            let besediloLat::[MarkupText]
                besediloLat = sezMarkTeksta
                    where
                        --da stran prvo in jo zamenja
                        (x:sez1) = (lines besedilo)
                        (y:sez2) = (lines besediloRezultat)
                        sez3 = ("Besedilo za urejanje:":sez1)++("*":"Izracunani podatki:":sez2)
                        sezMarkTeksta = concat [[prose x, newline]|x <- sez3]
                        
            urejevalnikLat # new besediloLat
        
            ooL2 <- newFrame oknoLat []
            grid ooL2 [GridPos (1,2),GridPadX 1,GridPadY 1]
        
            gumbL1 <- newButton ooL2 [text "Ovrednoti"]
            gumbL2 <- newButton ooL2 [text "Zapri urejanje"]
        
            klikgL1 <- clicked gumbL1
            klikgL2 <- clicked gumbL2
        
            let sezG = [gumbL1, gumbL2]
            -- Naslednja funkcija delayWish je pri hitrosti danasnjih racunalnikov mogoce odvec (vse gumbe da gor na enkrat)
            delayWish $ mapM_ (\(gumb,(x,y)) -> grid gumb [GridPos (x,y),GridPadX 1,GridPadY 1]) (zip sezG [(x,1)|x <-[1..100]])
            
            
            let ovrednotiLatNaSpremembah =
                    do
                    besediloSprememb <- (getValue urejevalnikLat) :: IO String
                    let [besediloSpremembe, besediloRezultatSpremembe] = split '*' besediloSprememb
                    narediLatDatIzVsega besediloSpremembe besediloRezultatSpremembe
            
            _ <- spawnEvent(forever (
                        (
                        klikgL1 >>> ovrednotiLatNaSpremembah
                        )
                        +>
                        (klikgL2 >>> destroy oknoLat)
                        )
                        )
                        
            done
    
    
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
    gumb7 <- newButton oo3 [text "Naredi PDF"]
    gumb8 <- newButton oo3 [text "Izpisuj kordinate"]
    --gumb9 <- newButton oo3 [text "Razresi relacije"]
    
    klikg1 <- clicked gumb1
    klikg2 <- clicked gumb2
    klikg3 <- clicked gumb3
    klikg4 <- clicked gumb4
    klikg5 <- clicked gumb5
    klikg6 <- clicked gumb6
    klikg7 <- clicked gumb7
    klikg8 <- clicked gumb8
    --klikg9 <- clicked gumb9

    let sezG = [gumb1,gumb8,gumb3,gumb4,gumb5,gumb6,gumb7,gumb2]
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
    
    -- let narisiObjekt (Kroznica (x,y) r) = (
                                        -- do
                                        -- objektNOV <- createOval platno [coord[(x1,y1),(x2,y2)], outline "grey"]
                                        -- objektNOV
                                        
                                        -- oznakaP <- createCanvasTag platno []
                                        -- addCanvasTag (withTag objektNOV) oznakaP
                                        -- return oznakaP
                                        -- )
            -- where 
                -- (x1,y1,x2,y2) = pretvoriIzStandardneOblike (Kroznica (x,y) r)
                

        -- narisiObjekt (Tocka (x1,y1)) = (
                                        -- do
                                        -- objektNOV <- createOval platno [coord[(x1a-1,y1a-1),(x1a+1,y1a+1)], outline "grey", filling "red"]
                                        -- objektNOV
                                        -- oznakaP <- createCanvasTag platno []
                                        -- addCanvasTag (withTag objektNOV) oznakaP
                                        -- return oznakaP
                                        -- )
            -- where
                -- x1a=(osnovniPretvornik4 x1) ::Distance
                -- y1a=(osnovniPretvornik4 y1) ::Distance
                
        -- narisiObjekt (Premica (x1,y1) (x2,y2)) = (
                                        -- do
                                        -- objektNOV <- createLine platno [coord[(x1a,y1a),(x2a,y2a)], outline "grey"]
                                        -- objektNOV
                                        -- oznakaP <- createCanvasTag platno []
                                        -- addCanvasTag (withTag objektNOV) oznakaP
                                        -- return oznakaP
                                        -- )
            -- where
                -- x1a=(osnovniPretvornik4 x1) ::Distance
                -- y1a=(osnovniPretvornik4 y1) ::Distance
                -- x2a=(osnovniPretvornik4 x2) ::Distance
                -- y2a=(osnovniPretvornik4 y2) ::Distance
                
    (klikMiskaM1, _) <- bind platno [WishEvent [] (ButtonPress (Just 1))]
    (premikPoKlikM1, _) <- bind platno [WishEvent [Button1] Motion]
    (odKlikMiskaM1, _) <- bindSimple platno (ButtonRelease (Just 1))
    (navadniPremik, _) <- bind platno [WishEvent [] Motion]
    
     
    let besediloOdvisnoOdObjekta oznakaP osObjekt =[ --sezOznakP = [
            action (
            do
            oznakaP # coord [(x0a,y0a), (x1a,y1a)]    
            done
            )
            [link (show osObjekt)]
            ,newline
            ]
            where
                (x0a,y0a,x1a,y1a) = (pretvoriIzStandardneOblike osObjekt)

            
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
            always (gumb8 # text ("(x,y): " ++ show (x,y) ++ "  (x0,y0): " ++ show (x0,y0)))
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
            -- iskaze se da je tako bolj tocno (move je diskretna funkcija)
            always(oznakaP # (spremembaObjekta nizObjekta x0 y0 x1 y1))
            
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
                
                oznakaP # (spremembaObjekta nizObjekta x0 y0 x1 y1)
                -- zato da vedno lahko vidimo zacetni priblizek (ob kliku se vedno ponastavi)
                done
                putStrLn nizOmejitev
                putStrLn besedilo
                
                let sezObjektov=poisciUstrezneKroge nizOmejitev besedilo (standardizirajObliko nizObjekta x0 y0 x1 y1)
                putStrLn (show (sezObjektov))
                
                -- mapM_ narisiObjekt sezObjektov
                -- mapM narisiObjektKroznica sezObjektov
                -- let sezOznakP = mapM narisiObjekt sezObjektov

                let sez =[prose "Tukaj se izpisejo rezultati izracunov:", newline] ++ concat (map (\z -> besediloOdvisnoOdObjekta oznakaP z) sezObjektov)
                -- ++ concat (map (\z -> besediloOdvisnoOdObjekta oznakaP z sezOznakP) sezObjektov)
                
                --[prose (unlines (map show (sezObjektov)))]
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
            always (gumb8 # text ("(x,y): " ++ show (x,y)))
            done
            )    
    
    _ <- spawnEvent(forever (
                        (klikg1 >> 
						do
						always(novMain)
						always(destroy main)
                        )
                        +>
                        (
                        klikg7 >>> oknoLat
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
                        -- always (gumb8 # text ("(x,y): " ++ show (x,y)))
                        -- done
                        -- )
                            )
                    )

    
    finishHTk
