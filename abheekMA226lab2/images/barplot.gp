set terminal pngcairo font "arial,10" size 1500,900
set output 'bar2_3.png'
set boxwidth 0.75
set style fill solid
set title "Probability in each interval for 100000 random numbers generated with Fibonacci Generator"
set ylabel "Probability"
set xlabel "Range of random numbers"
set yrange [0:0.06]
plot "bar2_3.txt" using 2:xtic(1) with boxes