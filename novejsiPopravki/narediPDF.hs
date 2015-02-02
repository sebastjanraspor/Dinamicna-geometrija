module NarediPDF where

import System.IO
import StrukturniObjekti

vTikzFormat :: OsnovniObjekt Double -> String
vTikzFormat (Tocka (a, b)) = concat ["|draw(",e,",",f,") node[below]{A};"]
    where
        [e,f]=map show (map (/100) [a,b])
        
vTikzFormat (Kroznica (a, b) r) = concat ["|draw(",e,",",f,") circle (",q,");"]
    where
        [e,f,q]=map show (map (/100) [a,b,r])
        
vTikzFormat (Premica (a, b) (c, d)) = concat ["|draw (",e,",",f,") -- (",g,",",h,");"] 
    where
        [e,f,g,h]=map show (map (/100) [a,b,c,d])

narediTikz :: [OsnovniObjekt Double] -> String
narediTikz sezObjektov = unlines (["|begin{tikzpicture}[scale=1]"]++odrezek++(map vTikzFormat sezObjektov)++["|end{tikzpicture}"])
    where
        odrezek = ["|path[use as bounding box] (0,0) rectangle (8,8);","|clip (0,0) rectangle ++(8,8);"]
        

narediLat :: [OsnovniObjekt Double] -> String
narediLat sezObjektov = unlines [glavaLat,"|begin{document}",(narediTikz sezObjektov),"|end{document}"]
    where
        glavaLat = unlines ["|documentclass{article}", "|usepackage[rgb]{xcolor}" , "|usepackage{tikz}", "|usepackage{verbatim}"]
        
-- ocitno je kako se spodnja funkcija posplosi
-- zamenjaj a za b
zamenjaj _ _ "" = ""
zamenjaj a b (x:niz)
    |x==a = b:(zamenjaj a b niz)
    |otherwise = x:(zamenjaj a b niz)

        
        
zapisiDat :: [OsnovniObjekt Double] -> IO ()
zapisiDat sezObjektov = do 
       izhod <- openFile "izhod.txt" WriteMode
       let niz = zamenjaj '|' '\\' (narediLat sezObjektov)
       hPutStrLn izhod niz
       hClose izhod

test00 = zapisiDat [Kroznica (200,270) 55, Premica (30,588.1441) (200,300), Kroznica (290,340.1441) 123, Tocka (177.1456, 177)]
-- \draw (3,0) arc (0:75:3cm);
-- \draw[red,thick,dashed] (2,2) circle (3cm);

-- \begin{figure}
-- \centering
-- \begin{tikzpicture}
-- <code>
-- \end{tikzpicture}
-- \caption{M1} \label{fig:M1}
-- \end{figure}

narediLatDatIzBesedila :: [Char] -> IO ()
narediLatDatIzBesedila besedilo =
        do
        zapisiDat sez2
    where
        sez1 = razdelitevPodatkov besedilo
        sez2 = [(read y):: OsnovniObjekt Double |(x:y:ostalo) <- sez1]
    
preberiRezultat besediloRezultat = (nizObjektOznaka, seznamObjektov)
    where
        sez1 = lines besediloRezultat
        nizObjektOznaka = (split ';' (sez1!!1))!!0
        (_:_:sez2) = sez1
        seznamObjektov = [(read y):: OsnovniObjekt Double |y <- sez2]
        --seznamObjektov = sez2
        
narediLatDatIzVsega :: [Char] -> [Char] -> IO ()
narediLatDatIzVsega besedilo besediloRezultat =
        do
        zapisiDat sez3
    where
        (nizObjektOznaka, seznamObjektov) = preberiRezultat besediloRezultat
        sez1 = razdelitevPodatkov besedilo
        sez2 = [(read y):: OsnovniObjekt Double|(x:y:ostalo) <- sez1, x/=nizObjektOznaka]
        sez3 = sez2++seznamObjektov
