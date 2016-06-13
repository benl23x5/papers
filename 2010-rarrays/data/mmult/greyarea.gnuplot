set title "on a 1.4Ghz UltraSPARC T2"

set xlabel "threads (on 8 PEs)"
set xrange [0:68]
set xtics  (1,8,16,24,32,40,48,56,64)

set ylabel "speedup"
set yrange [0:64]
set ytics  (0,8,16,24,32,40,48,56,64)

set terminal png giant size 500,300

set style line 1 lt -1 lw 1
set style line 2 lt 8 lw 2

set key off

plot x with lines ls 1, "greyarea-cut.ssv" using ($1):(91.614/$2):(91.614/$3):(91.614/$4) ls 2 with yerrorlines