set terminal latex
set output 'question3.tex'
set title "plot u(i) and u(i+1)"
set ylabel "$u(i+1)$"
set xlabel "$u(i)$ - $i$ th value generated"
set yrange [0:1]
set xrange [0:1]
plot "ques3.txt" using 1:2 notitle with points
