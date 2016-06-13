set title "Intel Lynnfield 4x2 2.8Ghz" 

set key on

set xlabel "threads (on 4 PEs)"
set xrange [0:8]
set xtics  (1,2, 3, 4, 5, 6, 7, 8)

set ylabel "runtime (ms)"
set yrange [0:500]

set terminal pdf monochrome dashed font "Times-Roman" fsize 12 size 5,4

set style line 1 lt 2 lw 1
set style line 2 lt 1 lw 4

plot "desire.ssv" using ($1):($3) title "Repa" ls 2  with lines, 44 title "OpenCV" with lines ls 1
