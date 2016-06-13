set title "Canny on 2xQuad-core 2.0GHz Intel Harpertown" 

set terminal epslatex size 3.5,2.3
set output "data/canny/tesla-1024.tex"

# Fix the graph box size
set lmargin at screen 0.12
set bmargin at screen 0.15
set rmargin at screen 0.9
set tmargin at screen 0.9

set key on

set xlabel "threads (on 8 PEs)"
set xrange [0.9:8.5]
set xtics  (1, 2, 3, 4, 5, 6, 7, 8) 

set ylabel "runtime (ms)"
set yrange [500:12000]
set ytics  	( "100"   100, "200"   200, ""      300,  "400"  400,      ""   500 \
		, ""      600, ""      700, "800"   800,  ""     900,  "1000"  1000 \
  		, "1000" 1000, "2000" 2000, ""     3000, "4000" 4000,      ""  5000 \
		, ""     6000, ""     7000, "8000" 8000, ""     9000, "10000" 10000)

set logscale y
set logscale x

set style line 1 lt 2 lw 1
set style line 2 lt 1 lw 4

set label "1024x1024 image" at 4,2800
set label " 768x768  image" at 4,1600
set label " 512x512  image" at 4,730


plot 	'data/canny/tesla.ssv' using ($1):($4) title "Safe Unrolled Stencil" ls 2  with lines, \
	'data/canny/tesla.ssv' using ($1):($3) title "" ls 2  with lines, \
	'data/canny/tesla.ssv' using ($1):($2) title "" ls 2  with lines, \
	3307 title "Single-threaded OpenCV" with lines ls 1, \
	1830  title "" with lines ls 1, \
	 851  title "" with lines ls 1