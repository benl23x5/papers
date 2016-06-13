set title "on 2x Quad-core 3GHz Xenon"

set key off

set xlabel "threads"
set xrange [0:8.5]
set xtics  (1,2,3,4,5,6,7,8)

set ylabel "speedup"
set yrange [0:5]
set ytics  (0,1,2,3,4,5)

set terminal png giant size 500,300

set style line 1 lt -1 lw 1
set style line 2 lt 8 lw 2

plot x with lines ls 1, "limitingfactor.ssv" using ($1):(8.7579/$2):(8.7579/$3):(8.7579/$4) ls 2  with yerrorlines
