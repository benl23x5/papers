
set title "Sparse Matrix-Vector Multiplication (1% non-zero elements)"

set terminal pdf size 4,2.8
set output "data/smvm.pdf"

set key on
set key left

set xlabel "Non-Zero Elements"
set logscale x
set xrange [100:40000000]

set xtics       ( "10" 10,      "100" 100,      "1k"   1000, "10k" 10000, "100k" 100000, \
                  "1M" 1000000, "10M" 10000000, "100M" 100000000)

set ylabel "Run Time"
set logscale y
set yrange [0.1:4000]

set ytics       ( "100us" 0.1, "1ms" 1,      "10ms"  10,      "100ms"  100,   "1s"   1000,  "10s" 10000)

set label " Out Of Memory\n(150k elements)" at 50000, 1437

plot    'data/smvm/smvm-OldNoRules.dat'   using ($2):($3) title "Baseline DPH"         ls 1  lw 4 with linespoints, \
        'data/smvm/smvm-New.dat'          using ($2):($3) title "New DPH"              ls 3  lw 4 with linespoints, \
        'data/smvm/smvm-Vector.dat'       using ($2):($3) title "Using Data.Vector"    ls 10 lw 4 with linespoints
