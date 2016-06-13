
set title "Radix Sort"

set terminal pdf size 3,2.2
set output "benchmarks/figs/radixsort/radixsort.pdf"

set key on
set key left

set xlabel "Elements (millions)"
set logscale x
set xrange [0.15:3]
set xtics  (0.1, 0.2, 0.4, 0.8, 1.6, 3.2)

set ylabel "Run Time (ms)"
set logscale y
set yrange [1:2000]

plot    'benchmarks/figs/radixsort/radixsort.dat' using ($1):($4)       \
                title "Accelerate -fusion -sharing"                     \
                ls 7  lw 4 with linespoints,                            \
        'benchmarks/figs/radixsort/radixsort.dat' using ($1):($3)       \
                title "... -fusion +sharing"                            \
                ls 2  lw 4 with linespoints,                            \
        'benchmarks/figs/radixsort/radixsort.dat' using ($1):($2)       \
                title "... +fusion +sharing"                            \
                ls 10 lw 4 with linespoints,                            \
        'benchmarks/figs/radixsort/radixsort.dat' using ($1):($6)       \
                title "Thrust"                                          \
                ls 1  lw 4 with linespoints
