import StrukturniObjekti
import Circle

test1=(tr_racunska_standrdna(tr_standardna_racunska (Kroznica (1,1) 3)) == (Kroznica (1,1) 3))
test2=(tr_racunska_standrdna(tr_standardna_racunska (Tocka (2,3))) == Tocka (2,3))
--test3 "odpove ker v nazaj pac dolocimo ene tocke na premici jaz sem izbral kot predstavnika premice k in (100, 100*k)."
test3=(tr_racunska_standrdna(tr_standardna_racunska (Premica (5,8) (1,-1))) == Premica (5,8) (1,-1))
test4=(tr_racunska_standrdna(tr_standardna_racunska (Premica (1,3) (2,6))) == tr_racunska_standrdna(tr_standardna_racunska (Premica (3,9) (-2,-6))))

k0 = Kroznica (209, 351) 42.5
     
k1 = Kroznica (115, 260) 72.11
k2 = Kroznica (190, 382) 35.44
k3 = Kroznica (330, 325) 31.24

--[p1,p2,p3] = take 3 tangent_to
p1 = tangent_to
p2 = tangent_to
p3 = tangent_to

vrniVseKrozniceKiZadoscajoBivsi k0 (k1,p1) (k2,p2) (k3,p3) = rrr
    where
        narediPogojNaOrentacijah (objekt, pogoj) = map pogoj (vrniOrentaciji objekt)
        [a,b,c]=(map narediPogojNaOrentacijah [(k1,p1),(k2,p2),(k3,p3)])
        [c01,c02] = vrniOrentaciji k0
        rrr01 = vrniVseKrozniceKiZadoscajoP c01 [a,b,c]
        rrr02 = vrniVseKrozniceKiZadoscajoP c02 [a,b,c]
        --rrr = rrr01 ++ rrr02
        --rrr = posebnoZdruzenjeSeznamov rrr01 rrr02
        
        rrr00 = map tr_racunska_standrdna (odstraniMaybe (rrr01 ++ rrr02))
        rrr = sortirajIzlociPodvojene rrr00

skoziPogoj (x,y) = passing_through x y
test00 = vrniVseKrozniceKiZadoscajoBivsi k0 (k1,p1) (k2,p2) (k3,p3)
test01 = vrniVseKrozniceKiZadoscajo k0 (map (\z -> [skoziPogoj z]) [(1,0),(5,4),(10,20)])

niz = " Kroznica (1,2) 5"
xxx = (read niz):: OsnovniObjekt Double
