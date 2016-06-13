set terminal pdf monochrome dashed font "Times-Roman" size 4.9,2.2

set terminal epslatex size 3.5,1.3
set output "data/sobel/smallness/histogram/sobel100-seq-iters100-lena512.tex"

# Fix the graph box size
set lmargin at screen 0.1
set bmargin at screen 0.25
set rmargin at screen 0.9
set tmargin at screen 0.9

set key off
set style line 1 lt 2 lw 1
set style line 2 lt 1 lw 4

set yrange [0:40]
set xrange [100:250]
set xlabel "runtime (ms)"

set label "100 consecutive runs of\nSobel stencil\nusing 6 threads on 8 PEs" at 620,18

plot 	'data/sobel/smallness/histogram/sobel100-seq-iters100-lena512.hist' using ($1+5):($3) with boxes