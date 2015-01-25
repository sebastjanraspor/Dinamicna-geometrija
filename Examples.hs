module Examples where
import Circle

c1 = clockwise_circle 115 260 72.11
c2 = clockwise_circle 190 382 35.44
c3 = clockwise_circle 330 325 31.24

c = find (anticlockwise_circle 209 351 42.5) $ satisfying (map tangent_to [c1, c2, c3])
d = find (clockwise_circle 209 351 42.5) $ satisfying (map tangent_to' [c1, c2, c3])

cc1 = line 227 299 530 384
cc2 = line (227+10) 299 (530+10) 384
--cc2 = line 263 376 348 268
tocka = (309, 321)

pre = intersection 309 321 cc1 cc2

circle_three_points :: Double -> Double -> Double -> Double -> Double -> Double -> Circle Known -> Circle Known
circle_three_points x1 y1 x2 y2 x3 y3 c0 = c
	where
	c = find c0 $ satisfying [passing_through x1 y1, passing_through x2 y2, passing_through x3 y3]


	
-- incircle :: Double -> Double -> Double -> Double -> Double -> Double -> Circle Known -> Circle Known
-- circle_three_points x1 y1 x2 y2 x3 y3 c0 = c
	-- where
	


-- wordsWhen     :: (Char -> Bool) -> String -> [String]
-- wordsWhen p s =  case dropWhile p s of
                      -- "" -> []
                      -- s' -> w : wordsWhen p s''
                            -- where (w, s'') = break p s'
-- split c = wordsWher (==c)
-- test = print $ split ',' "break,this,string,at,commas"

-- l1 = line (-1) (-1) (-1) 0
-- l2 = line (-1) 0 1 0
-- l3 = line 1 0 1 (-1)

l1 = line (-1) (0) (0) (1)
l2 = line (0) (1) (1) (0)
l3 = line (1) (0) (-1) (0)

c' = find (clockwise_circle 0 5 1) $ satisfying (map tangent_to [l1, l2, l3])
