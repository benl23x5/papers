
# set title "Fluid on 2xQuad-core 2.0GHz Intel Harpertown" 

set terminal epslatex size 3.5,2.3
set output "data/fluid/tesla-size.tex"

set key off

# Fix the graph box size
set lmargin at screen 0.12
set bmargin at screen 0.15
set rmargin at screen 0.9
set tmargin at screen 0.9

set ylabel      "relative runtime"

set xlabel      "matrix width"
set xtics       ( "64" 64,   "96" 96 \
                , "128" 128, "192" 192 \
                , "256" 256, "384" 384 \
                , "512" 512, "768" 768 \
                , "1024" 1024)
set logscale x
set yrange [0:2.5]

set style line 1 lt 3 lw 1
set style line 2 lt 1 lw 4

set label "C Gauss-Seidel" at 67,1.1
set label "C Jacobi"       at 67,0.65

set label "Repa -N1"       at 600,1.25 rotate by -10
set label "Repa -N2"       at 600,0.88 rotate by -10
set label "Repa -N4"       at 600,0.45  rotate by -10


plot    'data/fluid/tesla-size.ssv' using ($1):($2/$2) with lines title "CGS reference" ls 1, \
        'data/fluid/tesla-size.ssv' using ($1):($3/$2) with lines title "C-JC" ls 1, \
        'data/fluid/tesla-size.ssv' using ($1):($4/$2) with lines title "Repa -N1" ls 2, \
        'data/fluid/tesla-size.ssv' using ($1):($5/$2) with lines title "Repa -N2" ls 2, \
        'data/fluid/tesla-size.ssv' using ($1):($6/$2) with lines title "Repa -N4" ls 2


