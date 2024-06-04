./count_txt_files.sh
echo Expected: 0
touch peter.txt
./count_txt_files.sh
echo Expected: 1
touch paul.txt
touch mary.txt
./count_txt_files.sh
echo Expected: 3
