
set title "Tree Lookup"
set terminal pdf size 4,2.8
set output "data/indices.pdf"

set key on
set key right

set xlabel "Vector Length"
set logscale x
set xrange [800:2000000]

set ylabel "Run Time"
set logscale y
set yrange [1:20000]

set label "  Out Of Memory\n     (32k vector)" at 10000, 8000
set xtics  ( "10" 10,      "100"  100,      "1k"    1000,   "10k" 10000, "100k" 100000, "1M"  1000000)
set ytics  ( "1ms" 1,      "10ms"  10,      "100ms"  100,   "1s"   1000,  "10s" 10000, "100s" 100000)

plot    'data/indices/indices-Old.dat'    using ($1):($2) title "Baseline DPH"     ls 1  lw 4 with linespoints, \
        'data/indices/indices-New.dat'    using ($1):($2) title "New DPH"          ls 3  lw 4 with linespoints, \
        'data/indices/indices-Vector.dat' using ($1):($2) title "With Data.Vector" ls 10 lw 4 with linespoints

