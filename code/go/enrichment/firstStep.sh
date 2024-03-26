sed 's/\./\:/' topHits.txt > pointList



sed 's/\./\:/' ./*/*/*/topHits.txt > ./*/*/*/pointList



find . -path "topHits.txt" -type f -exec sed -i.bak "s/\./\:/" {} \;


#Final
find . -path "*topHits.txt" -type f -exec sed -i.bak "s/\./\:/g" {} \;
