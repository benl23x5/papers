set title "on a 1.4Ghz UltraSPARC T2"

set xlabel "threads (on 8 PEs)"
set xrange [0:17]
set xtics  (1,2,4,6,8,10,12,14,16)

set ylabel "speedup"
set yrange [0:10]
set ytics  0,2,10

set key off

set terminal png giant size 500,300

set style line 1 lt -1 lw 1
set style line 2 lt 8 lw 2
set style line 3 lt 6 lw 2

set label "size 300x300" at 12.0,9.1
set label "size 400x400" at 12.0,5.2

plot x with lines ls 1, "greyarea-300x300-cut.ssv" using ($1):(31552/$2):(31552/$3):(31552/$4) ls 2  with yerrorlines, "greyarea-400x400-cut.ssv" using ($1):(62.519/$2):(62.519/$3):(62.519/$4) ls 3 with yerrorlines