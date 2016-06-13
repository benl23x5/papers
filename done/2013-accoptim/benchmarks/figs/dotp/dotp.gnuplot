
set title "Dot product"

set terminal pdf size 3,2.2
set output "benchmarks/figs/dotp/dotp.pdf"

set key on
set key bottom

set xlabel "Elements (millions)"
set logscale x
set xrange [1.5:25]

set xtics (2, 4, 6, 8, 10, 12, 14, 16, 18, 20)

set ylabel "Run Time (ms)"
set logscale y
set yrange [0.05:100]

plot    'benchmarks/figs/dotp/dotp.dat' using ($1):($6)                 \
                title "Data.Vector"        ls 6  lw 4 with linespoints, \
        'benchmarks/figs/dotp/dotp.dat' using ($1):($7)                 \
                title "Repa -N8"           ls 3  lw 4 with linespoints, \
        'benchmarks/figs/dotp/dotp.dat' using ($1):($5)                 \
                title "NDP2GPU"            ls 4  lw 4 with linespoints, \
        'benchmarks/figs/dotp/dotp.dat' using ($1):($3)                 \
                title "Accelerate -fusion" ls 2  lw 4 with linespoints, \
        'benchmarks/figs/dotp/dotp.dat' using ($1):($2)                 \
                title "... +fusion"        ls 10  lw 4 with linespoints, \
        'benchmarks/figs/dotp/dotp.dat' using ($1):($4)                 \
                title "CUBLAS"             ls 1  lw 4 with linespoints

