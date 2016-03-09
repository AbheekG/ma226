set terminal latex
set output 'barchart5.tex'
set boxwidth 0.75
set style fill solid
set title "Range frequencies for 1000000 random numbers generated using LCG"
set ylabel "freq."
set xlabel "Range of random numbers"
set yrange [0:60000]
plot "ques2e.txt" using 2:xtic(1) with boxes