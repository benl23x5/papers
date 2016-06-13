
set title "Fluid Flow"

set terminal pdf size 3,2.2
set output "benchmarks/figs/fluid/fluid.pdf"

set key on
set key bottom

set xlabel "Image Size (total pixels)"
set logscale x
set xrange [5000:4000000]
set xtics \
        ( "1k"     1024,   "2k"   2048,  "4k"   4096, "8k"   8192 \
        , "16k"   16384,  "32k"  32768, "64k"  65536, "128k" 131072 \
        , "256k" 262144, "512k" 524288, "1M" 1048576, "2M"   2097152)

set ylabel "Run Time (ms)"
set logscale y
set yrange [0.5:10000]

plot    'benchmarks/figs/fluid/fluid.dat' using ($1):($5)       \
                title "C sequential"                            \
                ls 6  lw 4 with linespoints,                    \
        'benchmarks/figs/fluid/fluid.dat' using ($1):($4)       \
                title "Repa -N7"                                \
                ls 3  lw 4 with linespoints,                    \
        'benchmarks/figs/fluid/fluid.dat' using ($1):($3)       \
                title "Accelerate -sharing"                     \
                ls 7  lw 4 with linespoints,                    \
        'benchmarks/figs/fluid/fluid.dat' using ($1):($2)       \
                title "... +sharing"                            \
                ls 10  lw 4 with linespoints
