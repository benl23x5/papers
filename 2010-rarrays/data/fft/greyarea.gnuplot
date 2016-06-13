set title "on 1.4Ghz UltraSPARC T2"

set key off

set xlabel "threads (on 8 PEs)"
set xrange [0:34]
set xtics  (1,4,8,12,16,20,24,28,32)

set ylabel "speedup"
set yrange [0:16]
set ytics  (0,4,8,12,16)

set terminal png giant size 500,300

set style line 1 lt -1 lw 1
set style line 2 lt 8 lw 2

plot x with lines ls 1, "greyarea-cut.ssv" using ($1):(98.7778/$2):(98.7778/$3):(98.7778/$4) ls 2  with yerrorlines
