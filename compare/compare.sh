rm ./rapport/*
read stata < ../params/stata_path.dat
pwd=$(pwd)
pwd="$(pwd)/"
echo $pwd
$stata -b ../do/stats/profile_compas_table.do $1 $2 $pwd
$stata -b ../do/stats/profile_compas.do $1 $2 $pwd
echo "\centering" > titlepage.tex
echo "{\huge Rapport Benchmark }" >> titlepage.tex
echo " " >> title.tex
echo "{\Large Current : $1 }" >> titlepage.tex
echo " " >> title.tex
echo "{\Large Benchmark : $2 }" >> titlepage.tex
echo " " >> title.tex
echo "{\Large \today}" >> titlepage.tex
echo "\newpage" >> titlepage.tex
cp rapport.tex ./rapport/rapport.tex
cd rapport/
pdflatex --shell-escape rapport.tex
pdflatex --shell-escape rapport.tex 
cp rapport.pdf ../rapport_$1.pdf
