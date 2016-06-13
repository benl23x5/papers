
set title "N-Body"

set terminal pdf size 3,2.2
set output "benchmarks/figs/nbody/nbody.pdf"

set key on
set key left

set xlabel "Bodies"
set logscale x
set xrange [768:49152]

set xtics ("1k" 1024, "2k" 2048, "4k" 4096, "8k" 8192, "16k" 16384, "32k" 32768)

set ylabel "Run Time (ms)"
set logscale y
set yrange [0.05:6000]

plot    'benchmarks/figs/nbody/nbody-nofusionsharing.dat' using ($1):($2)      \
                title "Accelerate -fusion -sharing"                     \
                ls 7  lw 4 with linespoints,                            \
        'benchmarks/figs/nbody/nbody-nofusion.dat' using ($1):($2)      \
                title "... -fusion +sharing"                            \
                ls 2  lw 4 with linespoints,                            \
        'benchmarks/figs/nbody/nbody.dat' using ($1):($2)               \
                title "... +fusion +sharing"                            \
                ls 10  lw 4 with linespoints,                            \
        'benchmarks/figs/nbody/nbody.dat' using ($1):($3)               \
                title "CUDA"                                            \
                ls 1  lw 4 with linespoints                     
