
set title "Fluid on 2xQuad-core 2.0GHz Intel Harpertown foo" 

set terminal epslatex size 3.5,2.3
set output "data/fluid/tesla.tex"

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
set yrange [10:1000]
set ytics  	( "10"     10, "20"     20, ""       30,   "40"   40,      ""    50 \
		, ""       60, ""       70, "80"     80,  ""      90,   "100"   100 \
		, "100"   100, "200"   200, ""      300,  "400"  400,      ""   500 \
		, ""      600, ""      700, "800"   800,  ""     900,  "1000"  1000 \
  		, "1000" 1000, "2000" 2000, ""     3000, "4000" 4000,      ""  5000 \
		, ""     6000, ""     7000, "8000" 8000, ""     9000, "10000" 10000)

set logscale y
set logscale x

set style line 1 lt 2 lw 1
set style line 2 lt 1 lw 4

# set label "1024x1024 matrix" at 1.5,600 rotate by -10
# set label "512x512   matrix" at 1.5,150 rotate by -12
# set label "256x256   matrix" at 1.5,45  rotate by -8

plot 	'data/fluid/tesla.ssv' using ($1):($2)           title "" ls 2 with lines, \
	'data/fluid/tesla.ssv' using ($1):($3)           title "" ls 2 with lines, \
	'data/fluid/tesla.ssv' using ($1):(101.80/($1))  title "" ls 3 with lines, \
	'data/fluid/tesla.ssv' using ($1):(340.70/($1))  title "" ls 3 with lines

#	'data/fluid/tesla.ssv' using ($1):($2)           title "" ls 2 with lines, \
#	'data/fluid/tesla.ssv' using ($1):(44.200/($1))  title "" ls 3 with lines, \
#	'data/fluid/tesla.ssv' using ($1):(164.300/($1)) title "" ls 3 with lines, \
#	'data/fluid/tesla.ssv' using ($1):(670.200/($1)) title "" ls 3 with lines
	
	
