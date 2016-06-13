set title "on 2x Quad-core 3GHz Xenon"


set xlabel "threads"
set xrange [0:8.5]
set xtics  (1,2,3,4,5,6,7,8)

set ylabel "speedup"
set yrange [0:4]
set ytics  (0,1,2,3,4)

set terminal png giant size 500,300

set style line 1 lt -1 lw 1
set style line 2 lt 8 lw 2
set style line 3 lt 6 lw 2

set key off

set label "size 400x400" at 5.2,3.2
set label "size 300x300" at 5.2,2.0


plot x with lines ls 1 title "1:1", "limitingfactor-300x300.ssv" using ($1):(1.7299/$2):(1.7299/$3):(1.7299/$4) ls 2  with yerrorlines title "size 300x300", "limitingfactor-400x400.ssv" using ($1):(2.9708/$2):(2.9708/$3):(2.9708/$4) ls 3  with yerrorlines title "size 400x400"


