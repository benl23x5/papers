
set title "QuickHull benchmark"

set terminal pdf size 3,2.2
set output "data/quickhull/quickhull.pdf"

set key on
set key bottom

set xlabel "Input Size"
set logscale x
# set xrange [131072:67108864]
# set xtics  ("128K" 131072, "256K" 262144, "512K" 524288, "1M" 1048576, "2M" 2097152, "4M" 4194304, "8M" 8388608, "16M" 16777216, "32M" 33554432, "64M" 67108864)

# WHY is this not working???
# set xtics ("256K" 262144, "1M" 1048576, "4M" 4194304, "16M" 16777216)

set ylabel "Run Time (ms)"
set logscale y


plot    'data/quickhull/quickhull.dat' using ($1*$1):($2)             \
                title "Stream fusion"     ls 2  lw 4 with linespoints, \
        'data/quickhull/quickhull.dat' using ($1*$1):($3)             \
                title "Flow fusion"  ls 10  lw 4 with linespoints, \
        'data/quickhull/quickhull.dat' using ($1*$1):($4)             \
                title "Fallback"        ls 3  lw 4 with linespoints, \
        'data/quickhull/quickhull.dat' using ($1*$1):($5)             \
                title "Hand-fused C"        ls 1  lw 4 with linespoints
