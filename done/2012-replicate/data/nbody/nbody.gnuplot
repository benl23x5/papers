
set title "Barnes-Hut, 1 step, 1 thread"

set terminal pdf size 4,2.8
set output "data/nbody.pdf"

set key on
set key left

set xlabel "Bodies"
set logscale x
set xrange [6:120000]

set ylabel "Run Time"
set logscale y
set yrange [1:1000000]

set label "Out Of Memory\n (5000 bodies)" at 3000,200000

set xtics       ( "10" 10,      "100"  100,      "1k"    1000,   "10k" 10000, "100k" 100000)
set ytics       ( "1ms" 1,      "10ms"  10,      "100ms"  100,   "1s"   1000,  "10s" 10000, "100s" 100000)

plot    'data/nbody/nbody-NestedCopy.dat' using ($1):($2) title "Baseline DPH"  ls 1  lw 4 with linespoints, \
        'data/nbody/nbody-NestedVSeg.dat' using ($1):($2) title "New DPH"       ls 3  lw 4 with linespoints, \
        'data/nbody/nbody-Vector.dat' using ($1):($2) title "With Data.Vector" ls 10 lw 4 with linespoints
