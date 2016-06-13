
set title "Dot Product benchmark"

set terminal pdf size 3,2.2
set output "data/dotproduct/graph.pdf"

set key on
set key bottom

set xlabel "Input Size"
set logscale x
# set xrange [10000:100000000]
# set xtics  ("10^4" 10000, "10^5" 100000, "10^6" 1000000, "10^7" 10000000, "10^8" 100000000)

set ylabel "Run Time (ms)"
set logscale y


plot    'data/dotproduct/graph.dat' using ($1*$1):($2)             \
                title "Stream fusion"     ls 2  lw 4 with linespoints, \
        'data/dotproduct/graph.dat' using ($1*$1):($3)             \
                title "Flow fusion"  ls 10  lw 4 with linespoints, \
        'data/dotproduct/graph.dat' using ($1*$1):($4)             \
                title "Fallback"        ls 3  lw 4 with linespoints, \
