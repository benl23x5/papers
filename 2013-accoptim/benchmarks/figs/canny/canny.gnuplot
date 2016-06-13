
set title "Canny Edge Detection"

set terminal pdf size 3,2.2
set output "benchmarks/figs/canny/canny.pdf"

set key on
set key bottom

set xlabel "Image Size (total pixels)"
set logscale x
set xrange [40000:30000000]
set xtics  ("64k" 65536, "256k" 262144, "1M" 1048576, "4M" 4194304, "16M" 16777216)

set ylabel "Run Time (ms)"
set logscale y


plot    'benchmarks/figs/canny/canny.dat' using ($1*$1):($2)             \
                title "Accelerate (whole program)"     ls 2  lw 4 with linespoints, \
        'benchmarks/figs/canny/canny.dat' using ($1*$1):($3)             \
                title "Accelerate (just GPU kernels)"  ls 10  lw 4 with linespoints, \
        'benchmarks/figs/canny/canny.dat' using ($1*$1):($5)             \
                title "OpenCV (CPU)"        ls 3  lw 4 with linespoints, \
        'benchmarks/figs/canny/canny.dat' using ($1*$1):($6)             \
                title "OpenCV (GPU)"        ls 1  lw 4 with linespoints
