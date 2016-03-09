set terminal pngcairo font "arial,10" size 1500,900
set output 'q2.png'
set boxwidth 0.8
set style fill solid
set title "Gamma distribution with N = 5 and Lamda = 5"
set ylabel "Density"
set xlabel "Range of random numbers"
set yrange [0:200]
#set xrange [0:40]
plot "q2.dat" using 2:xtic(1) with boxes