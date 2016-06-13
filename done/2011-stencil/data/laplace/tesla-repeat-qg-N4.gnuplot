
set terminal pdf monochrome dashed font "Times-Roman" fsize 12 size 4.9,2

set terminal epslatex size 3.5,1.3
set output "data/laplace/tesla-repeat-qg-N4.tex"

# Fix the graph box size
set lmargin at screen 0.1
set bmargin at screen 0.25
set rmargin at screen 0.9
set tmargin at screen 0.9

set key off
set style line 1 lt 2 lw 1
set style line 2 lt 1 lw 4

set yrange [0:20]
set xrange [500:750]
set xlabel "runtime (ms)"
set ylabel "count"

set label "100 consecutive runs of\nSafe Unrolled Stencil solver\nusing 4 threads on 8 PEs" at 620,18

plot 	'data/laplace/tesla-repeat-qg-N4.hist' using ($1+5):($3) with boxes