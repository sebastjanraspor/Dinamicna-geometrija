import Text.Format
import Data.List
import OsnovneStrukture

toTikz :: Show a => OsnovniObjekt a -> String
toTikz (Tocka (a, b)) = 
    format "\\draw({0},{1})node[below]{$A$};" (map show [a,b])
toTikz (Kroznica (a, b) r) = 
    format "\\draw ({0},{1}) circle ({2});" (map show [a,b,r])
toTikz (Premica (a, b) (c, d)) = 
    format "\\draw ({0},{1}) -- ({2},{3});" (map show [a,b,c,d])

genTikz :: Show a => [OsnovniObjekt a] -> String
genTikz obj = 
    format "\\begin{tikzpicture}\n{0}\n\\end{tikzpicture}" [intercalate "\n" (map toTikz obj)]

genLatex :: Show a => String -> [OsnovniObjekt a] -> String
genLatex pr tz = format "{0}\n\\begin{document}\n{1}\n\\end{document}" [pr, genTikz tz]

preamble = "\\documentclass{article}\n\
\\\usepackage[rgb]{xcolor}\n\
\\\usepackage{tikz}\n\
\\\usepackage{verbatim}"

test = putStrLn (genLatex preamble [Kroznica (1,1) 1, Premica (1,1) (1,2)])
