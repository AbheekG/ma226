set terminal pngcairo font "arial,10" size 1000,1000
set output 'dataplot2c.png'
set title "plot u(i) and u(i+1) for 100000 values generated with fibonacci generator"
set ylabel "$u(i+1)$"
set xlabel "$u(i)$ - $i$ th value generated"
set yrange [0:1]
set xrange [0:1]
plot "dotplot2c.txt" using 1:2 notitle with dots
